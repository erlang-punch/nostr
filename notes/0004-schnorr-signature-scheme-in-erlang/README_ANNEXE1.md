# ANNEXE 1 - BIP-0340 Implementation in Python

Here the steps to extract and execute
[BIP-0340](https://github.com/bitcoin/bips/tree/master/bip-0340)
implementation in Python with debug mode activated.

```sh
git clone https://github.com/bitcoin/bips
cd bips/bip-0340
sed -i "s/^DEBUG = False/DEBUG = True/" reference.py
python reference.py
```

Here the output:

```
Test vector  #0:
   Variables in function schnorr_sign at line 118:
           msg == 0x0000000000000000000000000000000000000000000000000000000000000000
        seckey == 0x0000000000000000000000000000000000000000000000000000000000000003
      aux_rand == 0x0000000000000000000000000000000000000000000000000000000000000000
            d0 == 0x0000000000000000000000000000000000000000000000000000000000000003
             P == ('0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9',
                   '0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672')

             d == 0x0000000000000000000000000000000000000000000000000000000000000003
             t == 0x54f169cfc9e2e5727480441f90ba25c488f461c70b5ea5dcaaf7af69270aa517
            k0 == 0x1d2dc1652fee3ad08434469f9ad30536a5787feccfa308e8fb396c8030dd1c69
             R == ('0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca8215',
                   '0x849b08486a5b16ea5fd009a3ade472b48a2dc817aeebc33ab4fa25ebbd599f27')

             k == 0xe2d23e9ad011c52f7bcbb960652cfac815365cf9dfa59752c498f20c9f5924d8
             e == 0x6bb6b93a91f2ecc0cd924f4f9baabb5e6eb21745bb00f2cebdaac908bb5d86ce
           sig == 0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0
   Variables in function schnorr_verify at line 141:
           msg == 0x0000000000000000000000000000000000000000000000000000000000000000
        pubkey == 0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9
           sig == 0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0
             P == ('0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9',
                   '0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672')

             r == 0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca8215
             s == 0x25f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0
             e == 0x6bb6b93a91f2ecc0cd924f4f9baabb5e6eb21745bb00f2cebdaac908bb5d86ce
             R == ('0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca8215',
                   '0x7b64f7b795a4e915a02ff65c521b8d4b75d237e851143cc54b05da1342a65d08')

 * Passed signing test.
   Variables in function schnorr_verify at line 141:
           msg == 0x0000000000000000000000000000000000000000000000000000000000000000
        pubkey == 0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9
           sig == 0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0
             P == ('0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9',
                   '0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672')

             r == 0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca8215
             s == 0x25f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0
             e == 0x6bb6b93a91f2ecc0cd924f4f9baabb5e6eb21745bb00f2cebdaac908bb5d86ce
             R == ('0xe907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca8215',
                   '0x7b64f7b795a4e915a02ff65c521b8d4b75d237e851143cc54b05da1342a65d08')

 * Passed verification test.

Test vector  #1:
   Variables in function schnorr_sign at line 118:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        seckey == 0xb7e151628aed2a6abf7158809cf4f3c762e7160f38b4da56a784d9045190cfef
      aux_rand == 0x0000000000000000000000000000000000000000000000000000000000000001
            d0 == 0xb7e151628aed2a6abf7158809cf4f3c762e7160f38b4da56a784d9045190cfef
             P == ('0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659',
                   '0x2ce19b946c4ee58546f5251d441a065ea50735606985e5b228788bec4e582898')

             d == 0xb7e151628aed2a6abf7158809cf4f3c762e7160f38b4da56a784d9045190cfef
             t == 0x5966c1816cb27e627c8543d63478bdce03c1b115838e469a416b0899fe5723dd
            k0 == 0xf7becdac22c3d61a97ff4e84a004e1c4919c0e0c51f50dd5bee15c9cbd27318e
             R == ('0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de3341',
                   '0x20bc7663da14be43c22eb2ccb49cd746573b2766b277273fcb20a2f6c1a60c6c')

             k == 0xf7becdac22c3d61a97ff4e84a004e1c4919c0e0c51f50dd5bee15c9cbd27318e
             e == 0xcfb58e748d9648b71fdc909fb7432fc0c954da5bd75cdc9d4804d32648f9839a
           sig == 0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de33418906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a
   Variables in function schnorr_verify at line 141:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        pubkey == 0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659
           sig == 0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de33418906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a
             P == ('0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659',
                   '0x2ce19b946c4ee58546f5251d441a065ea50735606985e5b228788bec4e582898')

             r == 0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de3341
             s == 0x8906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a
             e == 0xcfb58e748d9648b71fdc909fb7432fc0c954da5bd75cdc9d4804d32648f9839a
             R == ('0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de3341',
                   '0x20bc7663da14be43c22eb2ccb49cd746573b2766b277273fcb20a2f6c1a60c6c')

 * Passed signing test.
   Variables in function schnorr_verify at line 141:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        pubkey == 0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659
           sig == 0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de33418906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a
             P == ('0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659',
                   '0x2ce19b946c4ee58546f5251d441a065ea50735606985e5b228788bec4e582898')
             r == 0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de3341
             s == 0x8906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a
             e == 0xcfb58e748d9648b71fdc909fb7432fc0c954da5bd75cdc9d4804d32648f9839a
             R == ('0x6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de3341', '0x20bc7663da14be43c22eb2ccb49cd746573b2766b277273fcb20a2f6c1a60c6c')
 * Passed verification test.

Test vector  #2:
   Variables in function schnorr_sign at line 118:
           msg == 0x7e2d58d8b3bcdf1abadec7829054f90dda9805aab56c77333024b9d0a508b75c
        seckey == 0xc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b14e5c9
      aux_rand == 0xc87aa53824b4d7ae2eb035a2b5bbbccc080e76cdc6d1692c4b0b62d798e6d906
            d0 == 0xc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b14e5c9
             P == ('0xdd308afec5777e13121fa72b9cc1b7cc0139715309b086c960e18fd969774eb8',
                   '0xf594bb5f72b37faae396a4259ea64ed5e6fdeb2a51c6467582b275925fab1394')
             d == 0xc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b14e5c9
             t == 0xbad44f6e1f50e3c2ad9d2ba5768ad6ab84dc4b8f9b893444234a9c39e8fc58e1
            k0 == 0xf5878384ed63c5ec428e7ab31bdb446b6884dfad76b7e0599af3f5e838409aab
             R == ('0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1b',
                   '0x5f193d22f6f1d925a7f8c4ceff20cc2a53ba1c3310ca843cf83156c1514bb284')
             k == 0xf5878384ed63c5ec428e7ab31bdb446b6884dfad76b7e0599af3f5e838409aab
             e == 0x9bc1ba4a0abbc0792066b2ca0ef771d88af676b322a83dd7517f7c1fd149215a
           sig == 0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1bab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7
   Variables in function schnorr_verify at line 141:
           msg == 0x7e2d58d8b3bcdf1abadec7829054f90dda9805aab56c77333024b9d0a508b75c
        pubkey == 0xdd308afec5777e13121fa72b9cc1b7cc0139715309b086c960e18fd969774eb8
           sig == 0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1bab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7
             P == ('0xdd308afec5777e13121fa72b9cc1b7cc0139715309b086c960e18fd969774eb8',
                   '0xf594bb5f72b37faae396a4259ea64ed5e6fdeb2a51c6467582b275925fab1394')
             r == 0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1b
             s == 0xab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7
             e == 0x9bc1ba4a0abbc0792066b2ca0ef771d88af676b322a83dd7517f7c1fd149215a
             R == ('0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1b',
                   '0x5f193d22f6f1d925a7f8c4ceff20cc2a53ba1c3310ca843cf83156c1514bb284')
 * Passed signing test.
   Variables in function schnorr_verify at line 141:
           msg == 0x7e2d58d8b3bcdf1abadec7829054f90dda9805aab56c77333024b9d0a508b75c
        pubkey == 0xdd308afec5777e13121fa72b9cc1b7cc0139715309b086c960e18fd969774eb8
           sig == 0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1bab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7
             P == ('0xdd308afec5777e13121fa72b9cc1b7cc0139715309b086c960e18fd969774eb8',
                   '0xf594bb5f72b37faae396a4259ea64ed5e6fdeb2a51c6467582b275925fab1394')
             r == 0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1b
             s == 0xab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7
             e == 0x9bc1ba4a0abbc0792066b2ca0ef771d88af676b322a83dd7517f7c1fd149215a
             R == ('0x5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1b',
                   '0x5f193d22f6f1d925a7f8c4ceff20cc2a53ba1c3310ca843cf83156c1514bb284')
 * Passed verification test.

...

Test vector #12:
   Variables in function schnorr_verify at line 134:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        pubkey == 0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659
           sig == 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f69e89b4c5564d00349106b8497785dd7d1d713a8ae82b32fa79d5f7fc407d39b
             P == ('0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659',
                   '0x2ce19b946c4ee58546f5251d441a065ea50735606985e5b228788bec4e582898')
             r == 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
             s == 0x69e89b4c5564d00349106b8497785dd7d1d713a8ae82b32fa79d5f7fc407d39b
 * Passed verification test.

Test vector #13:
   Variables in function schnorr_verify at line 134:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        pubkey == 0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659
           sig == 0x6cff5c3ba86c69ea4b7376f31a9bcb4f74c1976089b2d9963da2e5543e177769fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
             P == ('0xdff1d77f2a671c5f36183726db2341be58feae1da2deced843240f7b502ba659',
                   '0x2ce19b946c4ee58546f5251d441a065ea50735606985e5b228788bec4e582898')
             r == 0x6cff5c3ba86c69ea4b7376f31a9bcb4f74c1976089b2d9963da2e5543e177769
             s == 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
 * Passed verification test.

Test vector #14:
   Variables in function schnorr_verify at line 134:
           msg == 0x243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89
        pubkey == 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc30
           sig == 0x6cff5c3ba86c69ea4b7376f31a9bcb4f74c1976089b2d9963da2e5543e17776969e89b4c5564d00349106b8497785dd7d1d713a8ae82b32fa79d5f7fc407d39b
             P == None
             r == 0x6cff5c3ba86c69ea4b7376f31a9bcb4f74c1976089b2d9963da2e5543e177769
             s == 0x69e89b4c5564d00349106b8497785dd7d1d713a8ae82b32fa79d5f7fc407d39b
 * Passed verification test.

All test vectors passed.
```
