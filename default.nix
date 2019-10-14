{ mkDerivation, base, base16-bytestring, base64-bytestring
, bytestring, containers, deepseq, deepseq-generics, filepath
, HUnit, megaparsec, mtl, parser-combinators, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, xml
}:
mkDerivation {
  pname = "deseo";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring containers
    deepseq deepseq-generics megaparsec mtl parser-combinators
    QuickCheck xml
  ];
  testHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring containers
    filepath HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  description = "Asterix decoder/encoder";
  license = stdenv.lib.licenses.gpl3;
}
