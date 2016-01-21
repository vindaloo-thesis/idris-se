Idris to Serpent back end
-------------------------
Proof-of-concept compiler back end for smart contracts written in Idris. Compiles down to Serpent. Far from ready for production - output code exceeds gas limit even for very trivial programs. 


Examples
--------
https://github.com/vindaloo-thesis/examples


Build and install
-----------------
Tested with Idris version 0.9.20.1. Idris needs to be built with FFI support.

    cabal install
    cd lib && idris --build ethereum.ipkg -i lib --interface; cd ..
