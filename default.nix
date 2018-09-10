{ mkDerivation, base, bytestring, containers, deepseq
, deepseq-generics, filepath, HUnit, megaparsec, mtl, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, xml
}:
mkDerivation {
  pname = "deseo";
  version = "1.4.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deepseq deepseq-generics megaparsec mtl
    QuickCheck xml
  ];
  testHaskellDepends = [
    base bytestring containers filepath HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  description = "Asterix decoder/encoder";
  license = stdenv.lib.licenses.gpl3;
}
