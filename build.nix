{ mkDerivation, aeson, async, aeson-casing, base, bytestring, containers
, dhall, lib, optparse-applicative, prettyprinter, split, text
, turtle
}:
mkDerivation {
  pname = "dhall-terraform-libgen";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base containers dhall split text turtle
  ];
  executableHaskellDepends = [
    aeson async base bytestring containers dhall optparse-applicative
    prettyprinter text turtle
  ];
  testHaskellDepends = [ base ];
  description = "Creates Dhall Resources from terraform provider schemas";
  license = lib.licenses.mit;
}
