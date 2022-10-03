-- Álvaro Luís Silva Peixoto

type CadastroSUS = [Cidadao]

type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String 
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio,
               Estado, Telefone, Email)

------------ Cadastro geral para testes:

meuCadastro :: CadastroSUS
meuCadastro = [(87903452124, "Carla Santos", 'F', (27,4,1984), "Rua G, 123", "Sao Cristovao", "SE", "990845323", "carlasantos@gmail.com"), 
              (26716347665, "Alvaro Peixoto", 'M', (11,10,1996),"Rua C, 204", "Ilheus", "BA", "999997000", "alvarop@gmail.com"),
              (45839214032, "Jose Alves", 'M', (10,3,1970), "Rua Y, 234", "Sao Cristovao", "SE", "987894320", "josealves@yahoo.com"),
              (56789432109, "Fernada Souza", 'F', (5,4,1999), "Rua Z, 89", "Salvador", "BA", "980978543", "fernandasouza@gmail.com"),
              (32689043563, "Maria Teixeira", 'F', (13,9,1987), "Rua A, 67", "Sao Paulo", "SP", "99874321", "mariateixeira@outlook.com")
              ]
            
----------- Cadastros individuais para testes:

-------------- Inseridos no cadastro principal

alvaro :: Cidadao
alvaro = (26716347665, "Alvaro Peixoto", 'M', (11,10,1996),"Rua C, 204", "Ilheus", "BA", "999997000", "alvarop@gmail.com")

carla :: Cidadao
carla = (87903452124, "Carla Santos", 'F', (27,4,1984), "Rua G, 123", "Sao Cristovao", "SE", "990845323", "carlasantos@gmail.com")

jose :: Cidadao
jose = (45839214032, "Jose Alves", 'M', (10,3,1970), "Rua Y, 234", "Sao Cristovao", "SE", "987894320", "josealves@yahoo.com")

fernanda :: Cidadao
fernanda = (56789432109, "Fernada Souza", 'F', (5,4,1999), "Rua Z, 89", "Salvador", "BA", "980978543", "fernandasouza@gmail.com")

maria :: Cidadao
maria = (32689043563, "Maria Teixeira", 'F', (13,9,1987), "Rua A, 67", "Sao Paulo", "SP", "99874321", "mariateixeira@outlook.com")

----------- Fora do cadastro principal

joao :: Cidadao
joao = (34678932012, "Joao Pereira", 'M', (23,5,1993), "Rua K, 45", "Salvador", "BA", "998094567", "joaopereira@hotmail.com")


----------------------- Questão a)

----------------- Funções auxiliares

obterCPF :: Cidadao -> CPF
obterCPF (cpf,_,_,_,_,_,_,_,_) = cpf

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF cpf cadastro = [numeroDeCPF | numeroDeCPF <- cadastro, obterCPF numeroDeCPF == cpf] == []

--------------- Função principal

adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS cidadao cadastro
     | checaCPF (obterCPF cidadao) cadastro = cadastro ++ [cidadao]
     | otherwise                            = error "Esse CPF já está cadastrado no sistema."

--------------------- Questão b)

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS cpf cadastro novoEndereco =
    [alterarCidadao cpf cidadao novoEndereco | cidadao <- cadastro]
       where  
           alterarCidadao cpfProcurado (cpf, nome, genero, dataNasc, endereco, municipio,
            estado, telefone, email) novoEndereco
               | cpfProcurado == cpf = (cpf, nome, genero, dataNasc, novoEndereco, municipio,
                   estado, telefone, email)
               | otherwise             = (cpf, nome, genero, dataNasc, endereco, municipio,
                   estado, telefone, email)

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS cpf cadastro novoTelefone =
    [alterarCidadao cpf cidadao novoTelefone | cidadao <- cadastro]
       where  
           alterarCidadao cpfProcurado (cpf, nome, genero, dataNasc, endereco, municipio,
            estado, telefone, email) novoTelefone
               | cpfProcurado == cpf = (cpf, nome, genero, dataNasc, novoTelefone, municipio,
                   estado, telefone, email)
               | otherwise           = (cpf, nome, genero, dataNasc, endereco, municipio,
                   estado, telefone, email)

----------------- Questão c)

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS cpf cadastro 
     | not (checaCPF cpf cadastro) = [xs | xs <- cadastro, obterCPF xs /= cpf]
     | otherwise             = error "Este CPF não está contido no cadastro."

------------------ Questão d)

type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int

---------------------- Funções para obtenção de dados:

obterDataNasc :: Cidadao -> DataNasc 
obterDataNasc (_, _,_,nasc,_,_,_,_,_) = nasc


obterIdade1 :: Cidadao -> Data -> Int
obterIdade1 cidadao (dia1,mes1,ano1) 
    | mes1 >= mes && dia1 >= dia = ano1 - ano
    | mes1 > mes                 = ano1 - ano
    | otherwise = ano1 - ano - 1
    where (dia, mes, ano) = obterDataNasc cidadao


obterMunicipio :: Cidadao -> Municipio
obterMunicipio (_,_,_,_,_,municipio,_,_,_) = municipio

obterEstado :: Cidadao -> Municipio
obterEstado (_,_,_,_,_,_,estado,_,_) = estado

obterIdade :: Cidadao -> Int
obterIdade (_,_,_,(_,_,anoNasc),_,_,_,_,_) = 2021 - anoNasc

----------------------- Funções principais

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio cadastro municipio = length [ms | ms <- cadastro, obterMunicipio ms == municipio]

cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
cidadaosPorEstado cadastro estado = length [es | es <- cadastro, obterEstado es == estado]

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade cadastro municipio faixa = length [mis | mis <- cadastro, 
                                                             obterIdade mis >= fst faixa && obterIdade mis <= snd faixa,
                                                            obterMunicipio mis == municipio
                                                            ]

cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Quantidade
cidadaosPorEstadoIdade cadastro estado faixa = length [eis | eis <- cadastro, 
                                                      obterIdade eis >= fst faixa && obterIdade eis <= snd faixa,
                                                      obterEstado eis == estado
                                                      ]

---------------- Questão e)

------------------- Funções que exibem as listas em si

listaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> IO()
listaMunicipioFaixas cadastro municipio faixa = putStr ("MUNICIPIO:" ++ " " ++ municipio ++ "\n" ++ "FAIXAS DE IDADE" ++ concat 
                                                       (replicate 15 " ") ++ "QUANTIDADE" ++ "\n" ++ "\n" ++ formataLinhas 
                                                       (geraListaMunicipioFaixas cadastro  municipio faixa) ++ "\n" ++
                                                       "TOTAL" ++ formataTotal (geraListaMunicipioFaixas cadastro 
                                                       municipio faixa)
                                                       ) 

listaEstadoFaixas :: CadastroSUS -> Estado-> [FaixaIdade] -> IO()
listaEstadoFaixas cadastro estado faixa = putStr ("ESTADO:" ++ " " ++ estado ++ "\n" ++ "FAIXAS DE IDADE" ++ concat 
                                                 (replicate 15 " ") ++ "QUANTIDADE" ++ "\n" ++ "\n" ++ formataLinhas 
                                                 (geraListaEstadoFaixas cadastro  estado faixa) ++ "\n" ++
                                                  "TOTAL" ++ formataTotal (geraListaEstadoFaixas cadastro 
                                                 estado faixa)
                                                 ) 

------------------------- Funções para gerar as listas

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas cadastro municipio faixa = zip faixa [ cidadaosPorMunicipioIdade cadastro municipio x | x <- faixa]
  
geraListaEstadoFaixas :: CadastroSUS -> Estado -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaEstadoFaixas cadastro estado faixa = zip faixa [ cidadaosPorEstadoIdade cadastro estado x | x <- faixa]

------------- Questão f)
----------------------- Funções para realizar a formatação dos itens

----------------------- Formatação da quantidade de cidadãos por idade e município/estado

type QuantidadeFormatada = String

formataQuant :: Quantidade -> QuantidadeFormatada
formataQuant quantidade = concat (replicate (31 - length quantidadeFormat) " ") ++ quantidadeFormat
     where 
         quantidadeFormat = show quantidade

---------------------- Formatação de uma única linha

type LinhaFormatada = String

formataUmaLinha :: (FaixaIdade, Quantidade)-> LinhaFormatada
formataUmaLinha ((idadeInicial, idadeFinal), quantidade)
    | compi == 1 && compf == 1 = formatacaoPt1 ++ "    " ++ formatacaoPt2
    | compi == 1 && compf == 2 = formatacaoPt1 ++ "   " ++ formatacaoPt2
    | compi == 2 && compf == 2 = formatacaoPt1 ++ "  " ++ formatacaoPt2
    | compi == 2 && compf == 3 = formatacaoPt1 ++ " " ++ formatacaoPt2
    | otherwise                = formatacaoPt1 ++ formatacaoPt2
      where 
          formatacaoPt1 = show idadeInicial ++ " - " ++ show idadeFinal
          formatacaoPt2 = formataQuant quantidade
          compi         = length (show idadeInicial)
          compf         = length (show idadeFinal)

-------------- Formatação de todas as linhas

type LinhasFormatadas = String

formataLinhas :: [(FaixaIdade, Quantidade)] -> LinhasFormatadas
formataLinhas linhas = concat [formataUmaLinha x ++ "\n" | x <- linhas] 

-------------- Formatação do total das quantidades

type TotalFormatado = String
formataTotal :: [(FaixaIdade, Quantidade)] -> TotalFormatado
formataTotal linhas = concat (replicate (35 - length total) " ") ++ total ++ " "
    where 
        total = show (sum[quantidade | (faixaIdade, quantidade) <- linhas])

------------- Questão g)

type Vacinados = [Vacinado]
type Vacina = String
type TipoDose = Int
type Dose = (Vacina, Data)
type Doses = [Dose]
type Vacinado = (CPF, Doses)

----------------- Cadastros para testes:

listaDeVacinados :: Vacinados
listaDeVacinados = [(26716347665, [("CoronaVac",(22,09,2021)), ("CoronaVac", (13,10,2021))]), (56789432109, [("Pfizer",(10,07,2021))])]

fernandaVacinada :: Vacinado
fernandaVacinada = (56789432109, [("Pfizer",(10,07,2021))])

---------------------- Funções para obtenção e checagem de dados

localizarCidadao :: CPF -> CadastroSUS -> Cidadao
localizarCidadao cpf cadastro = head [x | x <- cadastro, obterCPF x == cpf]

obterCPFvacinado :: Vacinado -> CPF
obterCPFvacinado (cpf,_) = cpf

checaCPFvacinado :: CPF -> Vacinados -> Bool
checaCPFvacinado cpf vacinados = [numeroDeCPF | numeroDeCPF <- vacinados, obterCPFvacinado numeroDeCPF == cpf] /= []

checaCPFvacinado1 :: CPF -> Vacinados -> Vacinados
checaCPFvacinado1 cpf vacinados = [numeroDeCPF | numeroDeCPF <- vacinados, obterCPFvacinado numeroDeCPF == cpf]

checaIdadeVac :: CPF -> CadastroSUS -> FaixaIdade -> Bool
checaIdadeVac cpf cadastro (idadeInicial, idadeFinal) = [x | x <- cadastro, cpf == obterCPF x, 
                                                        obterIdade x >= idadeInicial && obterIdade x <= idadeFinal] /= []

checaMunicipio :: CPF -> CadastroSUS -> Municipio -> Bool
checaMunicipio cpf cadastro municipio = [x | x <- cadastro, obterMunicipio x == municipio] /= []

obterMunicipio2 :: CPF -> CadastroSUS -> Municipio
obterMunicipio2 cpf cadastro = obterMunicipio (localizarCidadao cpf cadastro)

aplicaPrimDose:: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpf cadastro faixa municipio vacina dataVac vacinados 
    | checaCPFvacinado cpf vacinados            = error "A primeira dose já foi aplicada."
    | checaCPF cpf cadastro                     = error "Este CPF não está contido no cadastro."
    | not (checaIdadeVac cpf cadastro faixa)    = error "Este cidadão não está na faixa de vacinação corrente."
    | obterMunicipio2 cpf cadastro /= municipio = error "Município não compatível. Atualize seus dados do SUS."   
    | vacina == "Janssen"                       = (cpf, [(vacina,dataVac), (vacina,dataVac)]) : vacinados
    | otherwise                                 = (cpf, [(vacina,dataVac)]) : vacinados

------------------ Questão h)

------------- Funções para obtenção e checagem de dados

checaSegDose :: CPF -> Vacinados -> Vacinados
checaSegDose cpf vacinados = [x | x <- vacinados, obterCPFvacinado x == cpf]

qtdDeDosesTomadas :: CPF -> Vacinados -> Int
qtdDeDosesTomadas cpf vacinados = length (head [x | (cpf1,x) <- vacinados, cpf == cpf1])

maiorData :: Data -> Data -> Bool
maiorData (dia1,mes1,ano1) (dia,mes,ano) 
     | ano1 > ano = True
     | ano < ano  = False
     | ano1 == ano && mes1 > mes = True
     | ano1 == ano && mes1 < mes = False
     | mes1 == mes && dia1 > dia = True
     | otherwise                 = False

geraDose :: CPF -> Vacinados -> Data -> Dose
geraDose cpf vacinados dataVac = (obterVacina cpf vacinados, dataVac)

obterDataPrimDose :: CPF -> Vacinados -> Data
obterDataPrimDose cpf vacinados = snd (head (snd (head (checaCPFvacinado1 cpf vacinados))))

obterDataSegDose :: CPF -> Vacinados -> Data
obterDataSegDose cpf vacinados = snd (last (snd (head (checaCPFvacinado1 cpf vacinados))))

listaSemOVacinado :: CPF -> Vacinados -> Vacinados
listaSemOVacinado cpf vacinados = [x | x <- vacinados, obterCPFvacinado x /= cpf]

obterVacina :: CPF -> Vacinados -> Vacina
obterVacina cpf vacinados = fst(head(snd(head(checaCPFvacinado1 cpf vacinados))))

------------------- Função principal

aplicaSegDose :: CPF -> Data -> Vacinados -> Vacinados
aplicaSegDose cpf dataVac vacinados
        | not (checaCPFvacinado cpf vacinados)  = error "Este CPF não está contido na lista de vacinados e não pode tomar a 2a dose."
        | qtdDeDosesTomadas cpf vacinados  == 2 = error "A segunda dose já foi aplicada a esta pessoa."
        | not (maiorData dataVac (obterDataPrimDose cpf vacinados)) 
                                                = error "A data informada não é posterior a data da 1a dose." 
        | otherwise                             = listaSemOVacinado cpf vacinados ++ 
                                                 [(cpf, [(vacina, dataPrimeiraDose), (vacina, dataVac)])
                                                 ]
             where 
                 vacina = obterVacina cpf vacinados
                 dataPrimeiraDose = obterDataPrimDose cpf vacinados

----------------- Questão i)

atualizaVacina :: CPF -> TipoDose -> Vacina -> Vacinados -> Vacinados
atualizaVacina cpf tipoDose vacinaCorreta vacinados
        | not (checaCPFvacinado cpf vacinados)  = error "Este CPF não está contido na lista de vacinados." 
        | tipoDose > numDose                    = error "Esta dose ainda não foi ministrada para este cidadão."
        | tipoDose == 1 && numDose == 1         = part1Lista ++ [(cpf, [(vacinaCorreta, dataPrimeiraDose)])]
        | otherwise                             = part1Lista ++ [(cpf, [(vacina, dataPrimeiraDose), 
                                                                (vacinaCorreta, dataSegDose)])
                                                                ]
           where
               part1Lista = listaSemOVacinado cpf vacinados
               numDose = length (snd(head(checaCPFvacinado1 cpf vacinados)))
               vacina = obterVacina cpf vacinados
               dataPrimeiraDose = obterDataPrimDose cpf vacinados
               dataSegDose = obterDataSegDose cpf vacinados

----------------- Questão j)

dosesJaTomadas :: CPF -> Vacinados -> TipoDose
dosesJaTomadas cpf vacinados = length (snd(head(checaCPFvacinado1 cpf vacinados)))

obterEstado2 :: CPF -> CadastroSUS -> Estado
obterEstado2 cpf cadastro = obterEstado (localizarCidadao cpf cadastro)

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun vacinados tipoDose municipio cadastro = length [x | x <- vacinados, obterMunicipio2 (fst x) cadastro == municipio,
                                                                 dosesJaTomadas (fst x) vacinados >= tipoDose
                                                                 ]

quantidadeDoseEst :: Vacinados -> TipoDose -> Estado -> CadastroSUS -> Quantidade
quantidadeDoseEst vacinados tipoDose estado cadastro = length [x | x <- vacinados, obterEstado2 (fst x) cadastro == estado,
                                                              dosesJaTomadas (fst x) vacinados >= tipoDose
                                                              ]
                                                                    
-------------- Questão k)
