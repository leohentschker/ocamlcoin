open Crypto
open Merkletree

module TestMerkle = MakeMerkle (TransactionSerializable) (SHA256) ;;

let _ = TestMerkle.run_tests ();;
