module Dictionary
    open System.Collections.Generic
    type Dict = 
    | Leaf of bool
    | Node of bool*Map<char, Dict>

    let empty () = Leaf false

    let rec insert (s:string) (d:Dict) =
        match d with
        // 
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (b,m) when s.Length = 0 -> Node (true,m)

        //
        | Leaf b -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) Map.empty)
        | Node (b,m) -> 
            match Map.tryFind s.[0] m with
                | Some d -> Node (b, Map.add (s.[0]) (insert s.[1..] d) m) 
                | None -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) m) 
        
