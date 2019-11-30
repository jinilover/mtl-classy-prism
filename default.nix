{ mkDerivation, aeson, attoparsec, base-noprelude, dhall
, email-validate, email-validate-json, exceptions, hedgehog
, http-client, http-client-tls, http-types, lens, mtl
, optparse-applicative, protolude, servant, servant-client, stdenv
, tagsoup, tasty, tasty-hedgehog, tasty-hunit, text
}:
mkDerivation {
  pname = "mtl-classy-prism";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base-noprelude dhall email-validate
    email-validate-json exceptions http-client http-client-tls
    http-types lens mtl optparse-applicative protolude servant
    servant-client tagsoup text
  ];
  testHaskellDepends = [
    aeson base-noprelude email-validate hedgehog lens servant-client
    tagsoup tasty tasty-hedgehog tasty-hunit text
  ];
  description = "Extract html content";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
