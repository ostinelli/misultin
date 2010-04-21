==========================================================================================================
DISCLAIMER
==========================================================================================================

Please note that the included certificate 'test_certificate.pem' and private key 'test_privkey.pem' are
publicly available via the Misultin repositories, and should NOT be used for any secure application. These
have been provided here for your testing comfort only.

You may consider getting your copy of OpenSSL <http://www.openssl.org> and generate your own certificate
and private key by issuing a command similar to:

openssl req -new -x509 -newkey rsa:1024 -days 365 -keyout test_privkey.pem -out test_certificate.pem
