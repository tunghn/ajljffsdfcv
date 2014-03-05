
type pTree = 
  | Leaf of (int * int)
  | Name of (string * int)
  | Node1 of (string * pTree * int)
  | Node2 of (string * pTree * pTree * int) 
  | Node3 of (string * pTree * pTree * pTree * int) ;;

