{ mkDerivation, base, bytestring, containers, deepseq
, deepseq-generics, filepath, HUnit, language-python, mtl
, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, xml
}:
mkDerivation {
  pname = "deseo";
  version = "1.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deepseq deepseq-generics language-python
    mtl QuickCheck xml
  ];
  testHaskellDepends = [
    base bytestring containers filepath HUnit language-python
    QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  description = "Asterix decoder/encoder";
  license = stdenv.lib.licenses.gpl3;
}
