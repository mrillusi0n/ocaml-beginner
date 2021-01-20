type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = {name = "Charizard"; hp = 78; ptype = Fire}
let metapod = {name = "Metapod"; hp = 50; ptype = Normal}
let pikachu = {name = "Pikachu"; hp = 64; ptype = Normal}

let pokemons = [charizard; metapod; pikachu]

let stronger_poke a b =
    if a.hp > b.hp then a else b

let rec max_hp = function
    | [] -> None
    | poke::pokes -> match max_hp pokes with
        | Some m -> Some (stronger_poke poke m)
        | None -> Some poke
