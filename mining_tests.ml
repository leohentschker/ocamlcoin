open TestHelpers
open Mining

let _ = Miner.test_mining () ;;
let _ = TestHelpers.run_tests Miner.test_mining ;;
print_endline ("Mining tests passed!")
