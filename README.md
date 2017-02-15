# Untyped REPL in Coq

## Prerequisites

    opam repo add coq-released https://coq.inria.fr/opam/released
    opam install -j4 -v coq-io-system

## Extract

    ./configure.sh
    make

## Build and Run

    cd extraction/
    make
    ./main.native

# Credits

* Maxim Sokhatsky
