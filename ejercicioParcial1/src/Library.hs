module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Gusto = String
type ListaGustos = [Gusto]

type Algo = String
type Medida = Number
type Miedo = (Algo,Medida)
type ListaMiedos = [Miedo]

data Persona = UnaPersona {
    gustos :: ListaGustos,
    miedos :: ListaMiedos,
    estabilidad :: Number
}deriving (Eq, Show)


maria = UnaPersona ["mecanica","mecanica","mecanica"] [("extraterrestres", 6000),("quedarse sin trabajo", 300)] 85

juanCarlos = UnaPersona ["maquillaje","trenes"] [("insectos", 100), ("coronavirus", 10),("vacunas",20)] 50

agregarMedida :: Algo -> Medida -> Miedo -> Miedo
agregarMedida algoNuevo medidaNueva (algo,medida)
    |algo == algoNuevo = (algo,medida+medidaNueva)
    |otherwise = (algo,medida)

volverseMiedoso :: Persona -> Miedo -> Persona
volverseMiedoso persona (nuevoAlgo,nuevaMedida) 
    |any ((==nuevoAlgo).fst) (miedos persona) = persona {miedos= map (agregarMedida nuevoAlgo nuevaMedida) (miedos persona)}
    |otherwise = persona {miedos= (nuevoAlgo,nuevaMedida):(miedos persona)}

perderMiedo :: Persona -> Algo -> Persona 
perderMiedo persona algoAPerder = persona {miedos= filter ((/=algoAPerder).fst) (miedos persona)}

variarEstabilidad :: Persona -> Number -> Persona
variarEstabilidad persona varianza  
    |(0<=(varianza+(estabilidad persona))) && ((varianza+(estabilidad persona))<=100) = persona {estabilidad = varianza+(estabilidad persona)}
    |otherwise = persona

volverseFan :: Persona -> Persona -> Persona
volverseFan persona idolo = persona {gustos = (gustos persona)++(gustos idolo)}

esFanatica :: Gusto -> Persona-> Bool
esFanatica gustoDado = (>2).length.(filter (==gustoDado)).gustos 

esMiedosa :: Persona -> Bool
esMiedosa= (>1000).(foldl1 (+)).(map(snd)).miedos