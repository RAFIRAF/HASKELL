import System.Environment
f[]=['Α'..'Ρ']++['Σ'..'Ω']
f _=['α'..'ρ']++['σ'..'ω']
main=f<$>getArgs>>=putStr