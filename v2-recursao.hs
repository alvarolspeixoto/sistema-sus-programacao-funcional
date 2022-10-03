-- Álvaro Luís Silva Peixoto

import Text.Printf
import Data.Char

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
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)

type Vacinados = [Vacinado]
type Vacina = String
type TipoDose = Int
type Dose = (Vacina, Data)
type Doses = [Dose]
type Vacinado = (CPF, Doses)

type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int
type Idade      = Int

------------ Cadastro geral para testes:

meuCadastro :: CadastroSUS
meuCadastro = [(87903452124, "Carla Santos", 'F', (27,4,1984), "Rua G, 123", "Sao Cristovao", "SE", "990845323", "carlasantos@gmail.com"), 
              (26716347665, "Alvaro Peixoto", 'M', (11,10,1996),"Rua C, 204", "Ilheus", "BA", "999997000", "alvarop@gmail.com"),
              (45839214032, "Jose Alves", 'M', (10,3,1970), "Rua Y, 234", "Sao Cristovao", "SE", "987894320", "josealves@yahoo.com"),
              (56789432109, "Fernada Souza", 'F', (5,4,1999), "Rua Z, 89", "Salvador", "BA", "980978543", "fernandasouza@gmail.com"),
              (32689043563, "Maria Teixeira", 'F', (13,9,1987), "Rua A, 67", "Sao Paulo", "SP", "99874321", "mariateixeira@outlook.com"),
              (53471688765, "Maria Silva", 'F', (21,12,2000),"Rua A, 202","Gloria", "SE", "999880300", "msilva@gmail.com"),
              (60710124535, "Dean Vinicius", 'M', (15,04,2003),"Rua A, 202","Aracaju", "SE", "998240073", "deanvi@gmail.com"),
              (53499988765, "Marcos Santos", 'M', (25,10,1994),"Rua D, 202","Aracaju", "SE", "999880501","msilva@gmail.com"),
              (85121388952, "Lucas Silva", 'M', (12,03,1999),"Rua D, 202","Aracaju", "SE", "999880501","lsilva@gmail.com"),
              (92234441237, "Gilson Mascarenhas", 'M', (12,03,1934),"Rua Juca Filho, 123","Aracaju", "SE", "999880501","gilson@yahoo.com")
              ]
            
----------- Cadastros individuais para testes:

-------------- Inseridos no cadastro principal:

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

----------- Fora do cadastro principal:

joao :: Cidadao
joao = (34678932012, "Joao Pereira", 'M', (23,5,1993), "Rua K, 45", "Salvador", "BA", "998094567", "joaopereira@hotmail.com")

----------- Questão 1.
-- a)
------- Função principal:

atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS _ [] _  = []
atualizaEndSUS cpf (x:xs) novoEndereco 
    | not (checaCPF cpf (x:xs)) = error "Esse CPF não está contido no cadastro."
    | obterCPF x == cpf = alteraEndereco x : removeLista cpf xs
    | otherwise         = x : atualizaEndSUS cpf xs novoEndereco
        where 
           alteraEndereco  (cpf, nome, genero, dataNasc, endereco, municipio, 
                                                      estado, telefone, email) 
            = (cpf, nome, genero, dataNasc, novoEndereco, municipio, estado, telefone, email)

-------- Funções auxiliares:

obterCPF :: Cidadao -> CPF
obterCPF (cpf,_,_,_,_,_,_,_,_) = cpf

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF _ [] = False
checaCPF cpf (x:xs)
  | cpf == obterCPF x = True
  | otherwise         = checaCPF cpf xs

removeLista :: CPF -> CadastroSUS -> CadastroSUS
removeLista _ [] = []
removeLista cpf (x:xs)
    | obterCPF x == cpf = xs
    | otherwise = x : removeLista cpf xs

-- b)
------- Função principal:

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS _ [] = []
removeSUS cpf (x:xs) 
   | not (checaCPF cpf (x:xs)) = error "Esse CPF não está contido no cadastro."
   | obterCPF x == cpf = xs
   | otherwise         = x : removeSUS cpf xs

-- c) 
--------------- Função principal:

geraListaMunicipioFaixas :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [(FaixaIdade, Quantidade)]
geraListaMunicipioFaixas cadastro municipio dataAtual faixa = zip faixa listaQtd
       where
         listaQtd = geraListaQtd cadastro municipio dataAtual faixa

------------- Funções auxiliares:

obterMunicipio :: Cidadao -> Municipio
obterMunicipio (_,_,_,_,_,municipio,_,_,_) = municipio

obterDataNasc :: Cidadao -> DataNasc 
obterDataNasc (_, _,_,nasc,_,_,_,_,_) = nasc

obterIdade :: Cidadao -> Data -> Int
obterIdade cidadao (dia1,mes1,ano1) 
    | mes1 >= mes && dia1 >= dia = ano1 - ano
    | mes1 > mes                 = ano1 - ano
    | otherwise = ano1 - ano - 1
    where (dia, mes, ano) = obterDataNasc cidadao

cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio -> Data -> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade [] _ _ _ = 0
cidadaosPorMunicipioIdade (x:xs) municipio dataAtual (idadeI, idadeF) 
     | obterMunicipio x == municipio && idade = 1 + cidadaosPorMunicipioIdade xs municipio dataAtual (idadeI, idadeF)
     | otherwise                     = cidadaosPorMunicipioIdade xs municipio dataAtual (idadeI, idadeF)
        where 
          idade = obterIdade x dataAtual >= idadeI && obterIdade x dataAtual <= idadeF

geraListaQtd :: CadastroSUS -> Municipio -> Data -> [FaixaIdade] -> [Quantidade]
geraListaQtd _ _ _ [] = []
geraListaQtd cadastro municipio dataAtual (y:ys) = quantidade y : geraListaQtd cadastro municipio dataAtual ys
        where 
          quantidade = cidadaosPorMunicipioIdade cadastro municipio dataAtual

----- d)
----------------- Cadastros para testes:

vacinados :: Vacinados
vacinados = [(26716347665, [("CoronaVac", (22,09,2021)), ("CoronaVac", (20,10,2021))]), (56789432109, [("Pfizer", (10,07,2021))]), 
                   (87903452124, [("AstraZeneca", (14,08,2021))]), (45839214032, [("Janssen", (30,06,2021)), ("Janssen", (30,06,2021))]),
                   (53471688765,[("Pfizer",(25,9,2021))]) ,(92234441237,[("Pfizer",(25,9,2021)),("Pfizer",(4,1,2022))]) ,
                   (85121388952,[("Pfizer",(25,9,2021))]),(53499988765,[("CoronaVac",(1,9,2021))]),(60710124535,[("Janssen",(25,9,2021)),("Janssen",(25,9,2021))])
                   ]

fernandaVacinada :: Vacinado
fernandaVacinada = (56789432109, [("Pfizer",(10,07,2021))])

--------- Função principal:

aplicaPrimDose :: CPF -> CadastroSUS -> FaixaIdade -> Municipio -> Vacina -> Data -> Vacinados -> Vacinados
aplicaPrimDose cpf cadastro (iI, iF) mun vac dataVac vacinados_
    | checaCPFvac cpf vacinados_ = error "A primeira dose já foi aplicada."
    | not (checaCPF cpf cadastro) = error "Esse CPF não está contido no cadastro."
    | idade < iI || idade > iF = error "Este cidadão não está na faixa de vacinação corrente."
    | obterMunicipio cidadao /= mun = error "Município não compatível. Atualize seus dados do SUS."
    | vac == "Janssen"                       = (cpf, [(vac,dataVac), (vac,dataVac)]) : vacinados_
    | otherwise                                 = (cpf, [(vac,dataVac)]) : vacinados_
    where
       idade = obterIdade cidadao dataVac
       cidadao = localizarCidadao cpf cadastro

------------- Funções auxiliares:

obterCPFvac :: Vacinado -> CPF
obterCPFvac (cpf,_) = cpf

localizarCidadao :: CPF -> CadastroSUS -> Cidadao
localizarCidadao _ [] = error "lista vazia"
localizarCidadao cpf (x:xs) 
    | cpf == obterCPF x = x
    | otherwise         = localizarCidadao cpf xs

checaCPFvac :: CPF -> Vacinados -> Bool
checaCPFvac _ [] = False
checaCPFvac cpf (x:xs)
   | obterCPFvac x == cpf = True
   | otherwise            = checaCPFvac cpf xs

--------- e)
--------- Função principal: 

quantidadeDoseMun :: Vacinados -> TipoDose -> Municipio -> CadastroSUS -> Quantidade
quantidadeDoseMun [] _ _ _ = 0
quantidadeDoseMun (x:xs) dose mun cadastro
   | dosesJaTomadas cpf (x:xs) >= dose &&
     obterMunicipio cidadao == mun = 1 + quantidadeDoseMun xs dose mun cadastro
   | otherwise                     = quantidadeDoseMun xs dose mun cadastro
  where
      cidadao = localizarCidadao cpf cadastro
      cpf = fst x

------ Funções auxiliares:

dosesJaTomadas :: CPF -> Vacinados -> TipoDose
dosesJaTomadas cpf vacinados_ = length(snd(obterVacinado cpf vacinados_))

obterVacinado :: CPF -> Vacinados -> Vacinado
obterVacinado _ [] = error "CPF não encontrado"
obterVacinado cpf (x:xs)
   | cpf == obterCPFvac x = x
   | otherwise            = obterVacinado cpf xs

-------------- f)
----- Função principal

quantidadeEstIdDose :: Vacinados -> Estado -> Data -> FaixaIdade -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstIdDose [] _ _ _ _ _ = 0
quantidadeEstIdDose (x:xs) est dataAtual (iI,iF) dose cadastro
    | dosesJaTomadas cpf (x:xs) >= dose &&
      obterEstado cidadao == est && idade >= iI && 
      idade <= iF = 1 + quantidadeEstIdDose xs est dataAtual (iI,iF) dose cadastro
    | otherwise   = quantidadeEstIdDose xs est dataAtual (iI,iF) dose cadastro
    where
      idade   = obterIdade cidadao dataAtual
      cidadao = localizarCidadao cpf cadastro
      cpf     = fst x

----- Funções auxiliares:

obterEstado :: Cidadao -> Municipio
obterEstado (_,_,_,_,_,_,estado,_,_) = estado

--------- g) 
------- Função principal: 

quantidadeEstVacDose :: Vacinados -> Estado -> Vacina -> TipoDose -> CadastroSUS -> Quantidade
quantidadeEstVacDose [] _ _ _ _  = 0
quantidadeEstVacDose (x:xs) est vac dose cadastro
   | dosesJaTomadas cpf (x:xs) >= dose &&
     obterEstado cidadao == est && 
     obterVacina x == vac = 1 + quantidadeEstVacDose xs est vac dose cadastro
   | otherwise            = quantidadeEstVacDose xs est vac dose cadastro
   where
    cidadao = localizarCidadao cpf cadastro
    cpf = fst x

------ Funções auxiliares:

obterVacina :: Vacinado -> Vacina
obterVacina (_,xs) = fst(head xs)

-------- Questão 2.
---------- Função principal:

faixasParaVacinar :: CadastroSUS -> Data -> Idade -> Municipio -> Quantidade -> (Idade, Idade)
faixasParaVacinar cadastro dataAtual idade mun qtd = (head listaFinal, last listaFinal)
   where 
     listaFinal    = calcularFaixa (ordenarDec (delimitaIdade (qtdIdade cadastro dataAtual mun listaDeIdades) idade)) qtd
     listaDeIdades = listaIdades cadastro dataAtual mun

--------- Funções auxiliares: 

pessoasPorIdade :: CadastroSUS -> Data -> Municipio -> Idade -> Quantidade
pessoasPorIdade [] _ _ _ = 0
pessoasPorIdade (x:xs) dataAtual mun idade
    | obterIdade x dataAtual == idade &&
      mun == obterMunicipio x = 1 + pessoasPorIdade xs dataAtual mun idade
    | otherwise               = pessoasPorIdade xs dataAtual mun idade

qtdIdade :: CadastroSUS -> Data -> Municipio -> [Idade] -> [(Quantidade, Idade)]
qtdIdade _ _ _ [] = []
qtdIdade xs dataAtual mun (y:ys) = (pessoasPorIdade xs dataAtual mun y, y) : qtdIdade xs dataAtual mun ys

listaIdades :: CadastroSUS -> Data -> Municipio -> [Idade]
listaIdades [] _ _ = []
listaIdades (x:xs) dataAtual mun
    | mun == obterMunicipio x = obterIdade x dataAtual : listaIdades xs dataAtual mun
    | otherwise               = listaIdades xs dataAtual mun

intercala :: [(Quantidade, Idade)] -> [(Quantidade, Idade)] -> [(Quantidade, Idade)]
intercala xs [] = xs
intercala [] ys = ys
intercala (x:xs) (y:ys) 
    | snd x >= snd y    = x : intercala xs (y:ys)
    | otherwise = y : intercala ys (x:xs)

ordenarDec :: [(Quantidade, Idade)] -> [(Quantidade, Idade)]
ordenarDec [] = []
ordenarDec [x] = [x]
ordenarDec xs = intercala (ordenarDec us) (ordenarDec vs)
  where
    meio = div (length xs) 2
    us   = take meio xs
    vs   = drop meio xs

delimitaIdade :: [(Quantidade, Idade)] -> Idade -> [(Quantidade, Idade)]
delimitaIdade [] _ = []
delimitaIdade (x:xs) idade
    | snd x < idade = x : delimitaIdade xs idade
    | otherwise     = delimitaIdade xs idade

calcularFaixa :: [(Quantidade, Idade)] -> Quantidade -> [Idade]
calcularFaixa [] _ = []
calcularFaixa (x:xs) qtd
   | fst x <= qtd = snd x : calcularFaixa xs (qtd - fst x)
   | otherwise    = calcularFaixa xs qtd

------------- Questão 3.

type Percentual = Float
type Populacao = Int
type PopMun = (Municipio, [(FaixaIdade, Populacao)])
type PopEstado = (Estado, [PopMun])
type PopPais = [PopEstado]

------- Cadastro populacional:

ajuPop :: PopMun
ajuPop = ("Aracaju",[((0, 10), 100), ((11, 20), 200), ((21, 30), 300), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])

gloPop :: PopMun
gloPop = ("Gloria",[((0, 10), 150), ((11, 20), 200), ((21, 30), 200), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])

murPop :: PopMun
murPop = ("Muribeca",[((0, 10), 150), ((11, 20), 200), ((21, 30), 200), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])
socPop :: PopMun
socPop = ("Socorro",[((0, 10), 150), ((11, 20), 200), ((21, 30), 200), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])
salPop :: PopMun
salPop = ("Salvador",[((0, 10), 100), ((11, 20), 200), ((21, 30), 300), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])
camPop :: PopMun 
camPop = ("Camacari",[((0, 10), 100), ((11, 20), 200), ((21, 30), 300), ((31,40), 400), ((41,50), 350), 
         ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
         ((111,120), 50), ((121,130), 50)])
saoPauloPop :: PopMun
saoPauloPop = ("Sao Paulo",[((0, 10), 100), ((11, 20), 200), ((21, 30), 300), ((31,40), 400), ((41,50), 350), 
        ((51,60), 250), ((61,70), 150), ((71,80), 100), ((81,90), 90), ((91,100), 50), ((101,110), 50),
        ((111,120), 50), ((121,130), 50)])

sePop :: PopEstado
sePop = ("SE", [socPop, ajuPop, murPop, gloPop])
baPop :: PopEstado
baPop = ("BA", [salPop, camPop])
spPop :: PopEstado
spPop = ("SP", [saoPauloPop])

paisPop :: PopPais
paisPop = [sePop, baPop, spPop]

-------------- Função principal:

percentualFaixas :: CadastroSUS -> Vacinados -> PopPais -> Estado -> TipoDose -> Data -> [(String, (Idade, Idade))]
percentualFaixas cadastro vac popPais est dose dt = zip listaDePercentuais faixas 
     where 
       listaDePercentuais = listaPercent faixas cadastro popPais vac est dose dt
       faixas = zip (0:[11, 21..121]) [10,20..130]

---------- Funções auxiliares:

qtdVacEstFaixa :: CadastroSUS -> Vacinados -> Estado -> TipoDose -> FaixaIdade -> Data -> Quantidade
qtdVacEstFaixa _ [] _ _ _ _ = 0
qtdVacEstFaixa cadastro (x:xs) est dose (iI, iF) dt
   | obterEstado cidadao == est &&
     idade >= iI && idade <= iF &&
     dose <= dosesJaTomadas cpf (x:xs) = 1 + qtdVacEstFaixa cadastro xs est dose (iI, iF) dt
   | otherwise                  = qtdVacEstFaixa cadastro xs est dose (iI, iF) dt
      where
        cidadao = localizarCidadao cpf cadastro
        cpf = fst x
        idade = obterIdade cidadao dt

popMunFaixa :: [(FaixaIdade, Populacao)] -> FaixaIdade -> Quantidade
popMunFaixa [] _ = 0
popMunFaixa (x:xs) (iI, iF) 
   | idadeI >= iI && idadeF <= iF = snd x + popMunFaixa xs (iI, iF)
   | otherwise                    = popMunFaixa xs (iI, iF)
    where 
      idadeI = fst (fst x)
      idadeF = snd (fst x)

unifica :: [PopMun] -> [(FaixaIdade, Populacao)]
unifica [] = []
unifica (x:xs) = snd x ++ unifica xs

popEstFaixa :: [PopMun] -> FaixaIdade -> Quantidade
popEstFaixa xs (iI, iF) = popMunFaixa (unifica xs) (iI, iF)

obterEstado3 :: PopPais -> Estado -> [PopMun]
obterEstado3 [] _ = error "Estado não contido no cadastro."
obterEstado3 (x:xs) est
   | fst x == est = snd x
   | otherwise = obterEstado3 xs est

percentual :: CadastroSUS -> PopPais -> Vacinados -> Estado -> TipoDose -> FaixaIdade -> Data -> Percentual
percentual cadastro popPais listaVac est dose faixa dt = fromIntegral (qtdVacEstFaixa cadastro listaVac est dose faixa dt) * 100 / 
                                                  fromIntegral (popEstFaixa (obterEstado3 popPais est) faixa)

formataPercent :: Float -> String
formataPercent x = printf "%.2f" x ++ "%"

listaPercent :: [FaixaIdade] -> CadastroSUS -> PopPais -> Vacinados -> Estado -> TipoDose -> Data -> [String]
listaPercent [] _ _ _ _ _ _ = []
listaPercent (x:xs) cadastro popPais vac est dose dt = formataPercent (percentual cadastro popPais vac est dose x dt) : 
                                                       listaPercent xs cadastro popPais vac est dose dt

------------- Questão 4.
----------- Função principal:

tabelaVacMun :: CadastroSUS -> Vacinados -> Estado -> PopPais -> IO()
tabelaVacMun cadastro vac est popPais = putStr ("PERCENTUAL DE VACINADOS POR MUNICIPIO" ++ "\n" ++ "\n" ++ "ESTADO: " ++ est ++ "\n" ++ 
                                        "MUNICIPIO" ++ concat (replicate 5 " ") ++ "% 1a dose" ++ concat (replicate 5 " ") ++ 
                                        "% 2a dose " ++ "\n" ++ formataLinhas (unifica3 listaMunicipios pct1 pct2)
                                               )
             where
               listaMunicipios = ordenaMun (listaMun (obterEstado3 popPais est))
               pct1 = listaPercentMun listaMunicipios cadastro (obterEstado3 popPais est) vac 1
               pct2 = listaPercentMun listaMunicipios cadastro (obterEstado3 popPais est) vac 2

-------------- Funções auxiliares:

qtdVacMunDose :: CadastroSUS -> Vacinados -> Municipio -> TipoDose -> Quantidade
qtdVacMunDose _ [] _ _ = 0
qtdVacMunDose cadastro (x:xs) mun dose
   | obterMunicipio cidadao == mun &&
     dosesJaTomadas cpf (x:xs) >= dose = 1 + qtdVacMunDose cadastro xs mun dose
   | otherwise                         = qtdVacMunDose cadastro xs mun dose
     where
        cidadao = localizarCidadao cpf cadastro
        cpf = fst x

obterMunicipio2 :: [PopMun] -> Municipio -> [(FaixaIdade, Populacao)]
obterMunicipio2 [] _ = error "Municipio não pertence a esse estado"
obterMunicipio2 (x:xs) mun
   | fst x == mun = snd x
   | otherwise    = obterMunicipio2 xs mun

popMunTotal :: [(FaixaIdade, Populacao)] -> Populacao
popMunTotal [] = 0
popMunTotal (x:xs) = snd x + popMunTotal xs

percentualMun :: CadastroSUS -> [PopMun] -> Vacinados -> Municipio -> TipoDose -> Percentual
percentualMun cadastro popEst vac mun dose = fromIntegral (qtdVacMunDose cadastro vac mun dose) * 100 /
                                             fromIntegral (popMunTotal (obterMunicipio2 popEst mun))

listaMun :: [PopMun] -> [Municipio]
listaMun [] = []
listaMun (x:xs) = fst x : listaMun xs

listaPercentMun :: [Municipio] -> CadastroSUS -> [PopMun] -> Vacinados -> TipoDose -> [String]
listaPercentMun [] _ _ _ _ = []
listaPercentMun (x:xs) cadastro popEst vac dose = formataPercent (percentualMun cadastro popEst vac x dose) : 
                                                  listaPercentMun xs cadastro popEst vac dose

unifica3 :: [Municipio] -> [String] -> [String] -> [((String, String), String)]
unifica3 xs ys zs = zip (zip xs ys) zs

formataUmaLinha :: ((String, String), String) -> String
formataUmaLinha ((cidade, pct1), pct2) = cidade ++ concat(replicate 8 " ") ++ pct1 ++ concat(replicate 8 " ") ++ pct2

formataLinhas :: [((String, String), String)] -> String
formataLinhas [] = ""
formataLinhas (x:xs) = formataUmaLinha x ++ "\n" ++ formataLinhas xs

--------- Algoritmo de ordenação (MergeSort): 

intercalaStr :: [Municipio] -> [Municipio] -> [Municipio]
intercalaStr xs [] = xs
intercalaStr [] ys = ys
intercalaStr (x:xs) (y:ys) 
   | x <= y    = x : intercalaStr xs (y:ys)
   | otherwise = y : intercalaStr ys (x:xs)
 
ordenaMun :: [Municipio] -> [Municipio]
ordenaMun [] = []
ordenaMun [x] = [x]
ordenaMun xs = intercalaStr (ordenaMun vs) (ordenaMun us)
   where
     meio = div (length xs) 2
     us   = take meio xs
     vs   = drop meio xs

------------- Questão 5.
------------- Função principal:

percentual2aDoseEst :: CadastroSUS -> Vacinados -> PopPais -> IO()
percentual2aDoseEst cadastro vac popPais = putStr ("PERCENTUAL COMPLETAMENTE IMUNIZADO" ++ "\n" ++ "\n" ++ "ESTADO" ++ concat (replicate 17 " ") 
                                                  ++ "PERCENTUAL" ++ " " ++ "\n" ++ formataLinhasEst lista)
                                          where 
                                            lista = ordenaEst (listaEstPercent estados percentuais)
                                            estados = listaEst popPais
                                            percentuais = listaPercent2 estados cadastro vac popPais

------------ Funções auxiliares:

qtd2aDoseEst :: CadastroSUS -> Vacinados -> Estado -> Quantidade
qtd2aDoseEst _ [] _ = 0
qtd2aDoseEst cadastro (x:xs) est
   | obterEstado cidadao == est &&
    dosesJaTomadas cpf (x:xs) == 2 = 1 + qtd2aDoseEst cadastro xs est 
   | otherwise                     = qtd2aDoseEst cadastro xs est
      where
        cidadao = localizarCidadao cpf cadastro
        cpf = fst x

popEstTotal :: [PopMun] -> Populacao
popEstTotal [] = 0
popEstTotal (x:xs) = popMunTotal (snd x) + popEstTotal xs

percentual2aDose :: CadastroSUS -> Vacinados -> PopPais -> Estado -> Percentual
percentual2aDose cadastro vac popPais est = fromIntegral (qtd2aDoseEst cadastro vac est) * 100 /
                                    fromIntegral (popEstTotal (obterEstado3 popPais est))

listaPercent2 :: [Estado] -> CadastroSUS -> Vacinados -> PopPais -> [Percentual]
listaPercent2 [] _ _ _ = []
listaPercent2 (x:xs) cadastro vac popPais = percentual2aDose cadastro vac popPais x : listaPercent2 xs cadastro vac popPais

listaEst :: PopPais -> [Estado]
listaEst [] = []
listaEst (x:xs) = fst x : listaEst xs

listaEstPercent :: [Estado] -> [Percentual] -> [(Estado, Percentual)]
listaEstPercent xs ys = zip xs ys

formataLinhaEst :: (Estado, Percentual) -> String
formataLinhaEst (est, pct) = est ++ concat (replicate 24 " ") ++ printf "%.2f" pct ++ "%"

formataLinhasEst :: [(Estado, Percentual)] -> String
formataLinhasEst [] = ""
formataLinhasEst (x:xs) = formataLinhaEst x ++ "\n" ++ formataLinhasEst xs


----- Algoritmo de ordenação (feito com inserção direta, enquanto o da questão 4 era feito com MergeSort):

insOrd :: (Estado, Percentual) -> [(Estado, Percentual)] -> [(Estado, Percentual)]
insOrd y [] = [y]
insOrd y (z:zs)
    | snd y >= snd z    = y : z : zs
    | otherwise = z : insOrd y zs

ordenaEst :: [(Estado, Percentual)] -> [(Estado, Percentual)]
ordenaEst [] = []
ordenaEst (x:xs) = insOrd x (ordenaEst xs)

------------- Questão 6.
{- Consultas escolhidas: 
  a) Dado um gênero e um estado, retornar a porcentagem de pessoas daquele gênero completamente imunizadas
  naquele estado.
  b) Dado um estado e uma dose, devolver o número de pessoas restantes a serem vacinadas com aquela dose naquele estado. 
-}
------ a)
-------- Função principal:

percentualEstGen :: CadastroSUS -> PopPais -> Vacinados -> Genero -> Estado -> String
percentualEstGen cadastro popPais vac gen est = formataPercent2 (fromIntegral (qtdEstGen cadastro vac gen est * 100) /
                                                fromIntegral (popEstTotal (obterEstado3 popPais est)))

------- Funções auxiliares:

formataPercent2 :: Float -> String
formataPercent2 x = printf "%.3f" x ++ "%"

obterGenero :: Cidadao -> Genero
obterGenero (_,_,genero,_,_,_,_,_,_) = genero

qtdEstGen :: CadastroSUS -> Vacinados -> Genero -> Estado -> Quantidade
qtdEstGen _ [] _ _ = 0
qtdEstGen cadastro (x:xs) gen est
  | obterEstado cidadao == est &&
    dosesJaTomadas cpf (x:xs) == 2 &&
    obterGenero cidadao == gen = 1 + qtdEstGen cadastro xs gen est 
  | otherwise                     = qtdEstGen cadastro xs gen est
        where
          cidadao = localizarCidadao cpf cadastro
          cpf = fst x

-------- b)
-------- Função principal: 

pessoasParaVacinar :: CadastroSUS -> PopPais -> Vacinados -> Estado -> TipoDose -> Quantidade
pessoasParaVacinar cadastro popPais vac est dose = popEstTotal (obterEstado3 popPais est) - qtdVacEstDose cadastro vac est dose

--------- Funções auxiliares:

qtdVacEstDose :: CadastroSUS -> Vacinados -> Estado -> TipoDose -> Quantidade
qtdVacEstDose _ [] _ _ = 0
qtdVacEstDose cadastro (x:xs) est dose
   | obterEstado cidadao == est &&
     dosesJaTomadas cpf (x:xs) >= dose = 1 + qtdVacEstDose cadastro xs est dose
   | otherwise                         = qtdVacEstDose cadastro xs est dose
     where
        cidadao = localizarCidadao cpf cadastro
        cpf = fst x