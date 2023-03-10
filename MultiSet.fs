//Remember to change file to .fs before handin

module MultiSet
    type MultiSet<'a when 'a : comparison> = MultiSet of Map<'a, uint32>
    let empty = MultiSet Map.empty
    let isEmpty (MultiSet ms) = ms.IsEmpty
    let size (MultiSet ms) = Map.fold (fun acc _ item -> acc + item) 0u ms
    let contains key (MultiSet ms) = ms.ContainsKey key
    let numItems key (MultiSet ms) = ms.TryFind key |> Option.defaultValue 0u
    let add key n (MultiSet ms) = MultiSet (ms.Add (key, (numItems key (MultiSet ms)+n)))
    let addSingle key (ms: MultiSet<'a>) = add key 1u ms
    let remove key n (MultiSet ms) = 
        let value = numItems key (MultiSet ms)
        if not (value = 0u) && value > n
        then MultiSet (ms.Add (key, value-n))
        else MultiSet (ms.Remove key)
    let removeSingle key (ms: MultiSet<'a>) = remove key 1u ms
    let fold f acc (MultiSet ms) = Map.fold f acc ms
    let foldBack f (MultiSet ms) acc = Map.foldBack f ms acc
    let ofList lst = List.fold (fun acc item -> addSingle item acc) empty lst
    let toList (ms: MultiSet<'a>) = fold (fun acc key item -> acc@[for _ in 1..(int item) -> key]) [] ms
    let map f (ms: MultiSet<'a>) = fold (fun acc key item -> add (f key) item acc) empty ms