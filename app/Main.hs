{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Concurrent.Async (mapConcurrently_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map, (!))
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import Data.Version (showVersion)
import qualified Dhall.Core as Dhall
import Dhall.Format (Format (..), format)
import qualified Dhall.Map
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified Options.Applicative as Opt
import Paths_dhall_terraform (version)
import Terraform.Convert
import Terraform.Types
import Turtle ((</>))
import qualified Turtle

-- | Pretty print dhall expressions.
pretty :: Pretty.Pretty a => a -> Text
pretty =
  PrettyText.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
    . Pretty.pretty

-- | Reads a JSON file that contains the schema definitions of a Terraform provider.
readSchemaFile :: FilePath -> IO ProviderSchemaRepr
readSchemaFile f = do
  doc <- (Aeson.eitherDecode <$> B.readFile f) :: IO (Either String ProviderSchemaRepr)
  case doc of
    Left e -> error e
    Right d -> pure d

getResources :: Text -> ProviderSchemaRepr -> Map Text SchemaRepr
getResources name schema = fromJust $ _resourceSchemas (_providerSchemas schema ! name)

getProvider :: Text -> ProviderSchemaRepr -> Map Text SchemaRepr
getProvider name schema =
  let provider = fromJust $ _provider (_providerSchemas schema ! name)
   in M.fromList [("provider", provider)]

getDataSources :: Text -> ProviderSchemaRepr -> Map Text SchemaRepr
getDataSources name schema = fromJust $ _dataSourceSchemas (_providerSchemas schema ! name)

-- | Write and format a Dhall expression to a file
writeDhall :: Turtle.FilePath -> Expr -> IO ()
writeDhall filepath expr = do
  putStrLn $ "Writing file '" <> Turtle.encodeString filepath <> "'"
  Turtle.writeTextFile filepath $ pretty expr <> "\n"
  format
    ( Format
        { chosenCharacterSet = Just Dhall.Pretty.Unicode,
          censor = Dhall.Util.NoCensor,
          outputMode = Dhall.Util.Write,
          transitivity = Dhall.Util.Transitive,
          inputs = Dhall.Util.InputFile (Turtle.encodeString filepath) :| []
        }
    )

-- | Generate a completion record for the resource.
mkRecord :: Turtle.FilePath -> Text -> BlockRepr -> IO ()
mkRecord rootPath name block = do
  let recordPath = rootPath </> Turtle.fromText (name <> ".dhall")
  let record =
        Dhall.RecordLit $
          Dhall.makeRecordField
            <$> Dhall.Map.fromList
              [ ("Type", mkBlockType block)
              , ("default", mkBlockDefault block)
              , ("Fields", mkBlockFields block)
              , ("showField", mkBlockShowField block)
              ]
  Turtle.mktree rootPath
  writeDhall recordPath record
  where
    mkBlockType :: BlockRepr -> Expr
    mkBlockType b = Dhall.Record $ Dhall.makeRecordField <$> Dhall.Map.fromList (typeAttrs b <> typeNested b)

    mkBlockDefault :: BlockRepr -> Expr
    mkBlockDefault b = Dhall.RecordLit $ Dhall.makeRecordField <$> Dhall.Map.fromList (defAttrs b <> defNested b)

    mkBlockFields :: BlockRepr -> Expr
    mkBlockFields b = Dhall.Union $ Nothing <$ (Dhall.Map.fromList (defAttrs b <> defNested b))

    mkBlockShowField :: BlockRepr -> Expr
    mkBlockShowField b =
      Dhall.Lam
         Nothing
         (Dhall.makeFunctionBinding "x" (mkBlockFields b))
         (Dhall.Merge
           (Dhall.RecordLit $
             Dhall.Map.fromList $
            (\(nm, _) -> (nm, Dhall.makeRecordField $ Dhall.TextLit (Dhall.Chunks [] nm))) <$>
            (typeAttrs b <> typeNested b))
           (Dhall.Var $ Dhall.V "x" 0)
           Nothing)

    defAttrs = attrs toDefault
    typeAttrs = attrs Just

    defNested = nested toDefault
    typeNested = nested Just

    attrs :: (Expr -> Maybe a) -> BlockRepr -> [(Text, a)]
    attrs mapExpr b =
      M.toList $
        M.mapMaybe mapExpr $
          M.map attrToType (fromMaybe noAttrs $ _attributes b)

    nested :: (Expr -> Maybe a) -> BlockRepr -> [(Text, a)]
    nested mapExpr b =
      M.toList $
        M.mapMaybe mapExpr $
          M.map nestedToType (fromMaybe noNestedBlocks $ _blockTypes b)

generate :: Turtle.FilePath -> Map Text SchemaRepr -> IO ()
generate rootDir schemas =
  mapM_
    (uncurry (mkRecord rootDir))
    blocks
  where
    blocks = M.toList $ M.map _schemaReprBlock schemas

data CliOpts = CliOpts
  { optSchemaFile :: String,
    optProviderName :: String,
    optOutputDir :: String
  }
  deriving (Show, Eq)

cliOpts :: Opt.Parser CliOpts
cliOpts =
  CliOpts
    <$> Opt.strOption
      ( Opt.long "schema-file"
          <> Opt.short 'f'
          <> Opt.help "Terraform provider's schema definitions"
          <> Opt.metavar "SCHEMA"
      )
    <*> Opt.strOption
      ( Opt.long "provider-name"
          <> Opt.short 'p'
          <> Opt.help "Which provider's resources will be generated"
          <> Opt.metavar "PROVIDER"
      )
    <*> Opt.strOption
      ( Opt.long "output-dir"
          <> Opt.short 'o'
          <> Opt.help "The directory to store the generated files"
          <> Opt.metavar "OUT_DIR"
          <> Opt.showDefault
          <> Opt.value "./lib"
      )

opts :: Opt.ParserInfo CliOpts
opts =
  Opt.info
    (Opt.helper <*> cliOpts)
    ( Opt.fullDesc
        <> Opt.progDesc "Generate Dhall types from Terraform resources"
        <> Opt.header ("dhall-terraform-libgen :: v" <> showVersion version)
    )

main :: IO ()
main = do
  parsedOpts <- Opt.execParser opts

  let outputDir = Turtle.fromText $ pack $ optOutputDir parsedOpts
      providerName = pack $ optProviderName parsedOpts
      mainDir = outputDir </> Turtle.fromText providerName
      providerDir = mainDir </> Turtle.fromText "provider"
      resourcesDir = mainDir </> Turtle.fromText "resources"
      dataSourcesDir = mainDir </> Turtle.fromText "data_sources"
      schema_generator = uncurry generate

  doc <- readSchemaFile (optSchemaFile parsedOpts)

  let generateDirs =
        [ (providerDir, getProvider providerName doc),
          (resourcesDir, getResources providerName doc),
          (dataSourcesDir, getDataSources providerName doc)
        ]

  mapConcurrently_ schema_generator generateDirs
