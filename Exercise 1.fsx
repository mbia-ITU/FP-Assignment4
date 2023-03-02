module MultiSet
    type MultiSet<'a when 'a : comparison> = MultiSet of Map<'a, uint32>
    let empty = MultiSet Map.empty
    let isEmpty (MultiSet ms) = ms.IsEmpty