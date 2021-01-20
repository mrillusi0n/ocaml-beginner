type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = {name = "Charizard"; hp = 78; ptype = Fire}
let metapod = {name = "Metapod"; hp = 50; ptype = Normal}
let pikachu = {name = "Pikachu"; hp = 64; ptype = Normal}

let pokemons = [charizard; metapod; pikachu]

let stronger_poke a b =
    if a.hp > b.hp then a
    else b

let max_hp = function
    | [] -> None
    | poke::pokes -> Some (List.fold_left stronger_poke poke pokes) 
