module Dictionary
    open System.Collections.Generic
    type Dict = 
    | Leaf of bool
    | Node of bool*Map<char, Dict>

    let empty () = Leaf false

    let rec insert (s:string) (d:Dict) =
        match d with
        // Word is done inserting and we are at the end of tree (Leaf)
        | Leaf _ when s.Length = 0 -> Leaf true
        // Word is done inserting, but we are NOT at the end of tree (Vi har insat abekat, og vil indsætte abe)
        | Node (b,m) when s.Length = 0 -> Node (true,m)

        (*Word is not done inserting, but we have reached end of tree therefore 
        we make the leaf into a node and insert a new leaf node so we can continue inserting 
        (Vi har insat abe, og vil indsætte abekat)*)
        | Leaf b -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) Map.empty)
        (*Check if letter of our word already exist in tree. 
        If yes use that letter chain. 
        If no create a new node*)
        | Node (b,m) -> 
            match Map.tryFind s.[0] m with
                | Some d -> Node (b, Map.add (s.[0]) (insert s.[1..] d) m) 
                | None -> Node (b, Map.add (s.[0]) (insert s.[1..] (empty())) m) 


    let rec lookup (s:string) (d:Dict) =
        match d with
            | Leaf b when s.Length = 0 -> b
            | Leaf _ -> false
            | Node (b,_) when s.Length = 0 -> b
            | Node (_ ,m) ->
                match m.TryFind(s.[0]) with
                    | Some d -> lookup s.[1..] d
                    | None -> false

    let step (c:char) (d:Dict) =
        match d with
        | Node (b, m) ->
            match Map.tryFind c m with
                | Some d -> 
                    match d with
                        | Leaf b -> Some (b, d)
                        | Node (b,m) -> Some (b, d)
                | None -> None
        | Leaf _ -> None      
