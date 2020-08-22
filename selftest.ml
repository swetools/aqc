open Oqc

module TrivialTest =
  struct
    let check () = Pass

    let description = "Trivial test"
  end ;;
register (module TrivialTest) ;;

run ();
