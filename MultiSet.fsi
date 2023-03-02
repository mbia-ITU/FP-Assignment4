module MultiSet
    type MultiSet<'a when 'a : comparison>
    val empty : MultiSet<'a>
    val isEmpty : MultiSet<'a> -> bool 