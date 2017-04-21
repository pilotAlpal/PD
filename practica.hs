--el test se representa: [(valor,numero de soluciones,respuesta correcta)]
data Test = Control [(Float,Int,Int)] deriving (Show)
--dato que representa una version del test como: test,modelo
data Examen =Forma Test Int deriving (Show)
--representa la respuesta de un alumno al examen: "nombre",modelo,[respuestas] 
data RespuestaTest = Entrega [Char] Int [Int] deriving (Show)
--recoje una serie de datos sobre un test ya evaluado: nombre,pTotal,pSobre10,nPreguntas,acertadas,nBlancas,[acertadas],[erroneas],[blancas]
data Correccion = Corrije [Char] Float Float Int Int Int [Int] [Int] [Int] deriving (Show)
--representa una tupla de frecuencias:(correctas,erroneas,blancas)
data Frecuencias = Porcentajes (Float,Float,Float) deriving (Show)
--recoje puntuacion media,medirespondidas,nsuspensos,aprobados,notables y sobresalientes,frecuencias absolutas y relativas mejores y peoeres 
--resultados,mas y menos veces en blanco
data Estadisticas = Datos Float Float Int Int Int Int Frecuencias [(Int,Frecuencias)] Int Int Int Int deriving (Show)
--oredena un test respecto al del profesor
modelaTest::RespuestaTest->RespuestaTest
modelaTest (Entrega name 1 xs)=(Entrega name 1 xs)
modelaTest (Entrega name 2 xs)=(Entrega name 1 (reverse xs))
--añdir modelo 2,modelo 3..
--funcion que se encarga de evaluar un test
--Parametros: test,respuestas,pAcum,pMax,nPreguntas,acertadas,nBlancas,[acertadas],[erroneas],[blancas]    
corriGG::Test->RespuestaTest->Float->Float->Int->Int->Int->[Int]->[Int]->[Int]->Correccion
corriGG  (Control [] )  (Entrega nombre  _ []) pAcumulado pPosible preguntas acertadas blancas as es bs = Corrije nombre pAcumulado (10*(pAcumulado/pPosible)) preguntas acertadas blancas as es bs
corriGG (Control ((v,a,cC):xs)) (Entrega nombre m  (cR:rs)) p pP preguntas acertadas blancas as es bs 
     | cC==cR  =corriGG (Control xs) (Entrega nombre m rs) (p+v) (pP+v) (preguntas+1) (acertadas+1) blancas (preguntas:as) es bs
     | cC==0 = corriGG (Control xs) (Entrega nombre m rs) p (pP+v) (preguntas+1) acertadas (blancas+1) as es (preguntas:bs)
     |otherwise = corriGG (Control xs) (Entrega nombre m rs) (p-(1/((fromIntegral a)-1))) (pP+v) (preguntas+1) acertadas blancas as (preguntas:es) bs
--funcion que cuenta el numero de veces que aparece una instancia de Eq en una lista
cuenta::Eq a=>a->[a]->Int
cuenta _ [] = 0
cuenta y (x:xs)
   |y==x = (1+cuenta x xs)
   |otherwise = cuenta x xs
--funcion que devuelve el valor mas repetido en una lista de enteros
moda :: [Int]->Int
moda []=(-1)
moda [x] = x
moda (x:xs)
   | (1+(cuenta x xs)) >(cuenta (moda xs) xs) = x
   | otherwise = moda xs
--funcion que devuelve el valor menos repetido (que aparece)en una lista de enteros
antiModa :: [Int]->Int
antiModa (x:xs)
  | xs==[] =x
  | (1+(cuenta x xs)) <(cuenta (antiModa xs) xs) = x
  | otherwise = antiModa xs
antiModa [] = (-1)

--divide un float entre un int y devuelve un float
floatBint::Float->Int->Float
floatBint x y = x/a where a = fromIntegral y::Float
--devuelve como Float la division de dos enteros
divFloat::Int->Int->Float
divFloat x y = fromIntegral(div x y)
--el tipo correccion recoje el resultado de corregir la respuesta a un test dada por el alumno
corrige::Test-> RespuestaTest -> Correccion
corrige t rt = corriGG t (modelaTest rt) 0 0 1 0 0 [] [] []
--listaFrecuencias [acers] [errs] [bls] correjidas cont
--dadas las listas de todas las preguntas acertadas,erroneas y en blanco,el numero de preguntas y un contador inicializado al numero de preguntas,
--obtiene una lista con las frecuencias de cada pregunta
listaFrecuencias::[Int]->[Int]->[Int]->Int->Int->[(Int,Frecuencias)]
listaFrecuencias _ _ _ _ 0 = []
listaFrecuencias as es bls nP c=(c,Porcentajes (porcentaje nP c as,porcentaje nP c es,porcentaje nP c bls)):listaFrecuencias as es bls nP (c-1)
--devuelve el porcentaje de veces que aparece un entero en una lista
--parametros:tamaño de la lista,valor a buscar,lista
porcentaje::Int->Int->[Int]->Float
porcentaje t i as = fromIntegral(div (cuenta i as) t)
--genera una tupla con las frecuencias de acertados erroneos y blancos
--parametros:acertadas,erroneas,blancas,evaluadas
porcentajes::Int->Int->Int->Int->Frecuencias
porcentajes a e b v = Porcentajes ((divFloat a v),(divFloat e v),(divFloat b v)) 
--genera el tipo que recoje las estadisticas
--Parametros:sumaPuntos,respondidos,suspensos,aprobados,ntbles,sobres,ace,err,bla,evaluadas,[as],[es],[bs] 
generaInforme ::Float->Int->Int->Int->Int->Int->Int->Int->Int->Int->[Int]->[Int]->[Int]-> Estadisticas
generaInforme sPuntos correjidos suspensos aprobados notables sobresalientes acertadas erroneas blancas evaluadas as es bs = 
 Datos (floatBint sPuntos correjidos) (divFloat(acertadas+blancas) correjidos) suspensos aprobados notables sobresalientes(porcentajes acertadas erroneas blancas evaluadas) (listaFrecuencias as es bs evaluadas evaluadas)(moda as) (antiModa as) (moda bs) (antiModa bs)
--devuelve para un test y una lista de respuestas un tipo que recoje una serie de estadisticas
estadisticas::Test->[RespuestaTest]->Estadisticas
estadisticas t rs = corrigeLista(formaLista t rs) 0 0 0 0 0 0 0 0 0 0 [] [] [] 
--dado un test y una lista de respuestas,devuelve una lista con las evaluaciones de cada test
formaLista::Test->[RespuestaTest]->[Correccion]
formaLista _ [] = []
formaLista t [r]=[corrige t r]
formaLsta t (r:rs)=(corrige t r) : (formaLista t rs) 
--procesa una lista de calificaciones y genera estadisticas
--Parametros: lista correciones,sPunts,resps,sus,aprbs,nts, sbs,acers,errs,blas,evas,[as][es][bs]
corrigeLista::[Correccion]->Float->Int->Int->Int->Int->Int->Int->Int->Int->Int->[Int]->[Int]->[Int]->Estadisticas
corrigeLista [] sP rr ss aa nn sb ac er bl ev as es bs = generaInforme sP rr ss aa nn sb ac er bl ev as es bs
corrigeLista ((Corrije _ pT p10 nPr ac bl as es bs):cs) sTot nRes sus aps nts sbs acT eT bT evas aTs eTs bTs 
  | (p10 >= 8.5) = corrigeLista cs (sTot + p10) (nRes+nPr-bl) sus aps nts (sbs+1) (acT+ac) (eT+nPr-ac-bl) (bT+bl) (evas+1) (as++aTs) (es++eTs) (bs++bTs)
  | (p10 >= 6.5) = corrigeLista cs (sTot + p10) (nRes+nPr-bl) sus aps (nts+1) sbs (acT+ac) (eT+nPr-ac-bl) (bT+bl) (evas+1) (as++aTs) (es++eTs) (bs++bTs)
  | (p10 >= 5.0) = corrigeLista cs (sTot + p10) (nRes+nPr-bl) sus (aps+1) nts sbs (acT+ac) (eT+nPr-ac-bl) (bT+bl) (evas+1) (as++aTs) (es++eTs) (bs++bTs)
  | otherwise = corrigeLista cs (sTot + p10) (nRes+nPr-bl) (sus+1) aps nts sbs (acT+ac) (eT+nPr-ac-bl) (bT+bl) (evas+1) (as++aTs) (es++eTs) (bs++bTs)
--lee un examen introducido por el usuario
leeExamen ::Int->IO [(Float,Int,Int)]
leeExamen 0 = return []
leeExamen n = do
    putStrLn "¿Cuantas opciones hay?"
    choices<-getLine
    putStrLn "¿Cual es la correcta?"
    correct<-getLine
    putStrLn "¿Cuanto vale la pregunta?"
    value<-getLine
    let vl=read value::Float
    let co=read correct::Int
    let ch= read choices::Int
    rest<-leeExamen (n-1)
    return ((vl,ch,co):rest) 
--lee las respuestas de un alumno
leeRespuestas::Int->Int->IO [RespuestaTest]
leeRespuestas 0 _ =return []
leeRespuestas a r= do
  c<-respuestasDeA r 
  cs<-leeRespuestas (a-1) r
  return (c:cs)
respuestasDeA ::Int->IO RespuestaTest
respuestasDeA n = do
  putStrLn "¿Cómo se llama el alumno?"
  name<-getLine
  putStrLn "¿Qué modelo de examen tiene?"
  modelo<-getLine
  let m=read modelo::Int
  a<-readAnswers name n m []
  return a
readAnswers::[Char]->Int->Int->[Int]->IO RespuestaTest
readAnswers name 0 m xs= return (Entrega name m xs) 
readAnswers name pr m xs =do
  putStrLn "¿Cual es la respuesta del alumno?"
  answer<-getLine
  let a=read answer::Int
  let as=a:xs
  respuestas<-readAnswers name (pr-1) m as
  return respuestas
--método principal
main=do
  putStrLn "¿Cuantas preguntas tiene el examen?"
  respuestas <-getLine
  let rr = read respuestas::Int
  tst<-leeExamen rr 
  let test=Control tst
  putStrLn "¿Cuantos alumnos se presentan?"
  nAlumnos<-getLine
  let nA=read nAlumnos::Int
  respuestas<-(leeRespuestas nA rr)
  let es=estadisticas test respuestas
  putStrLn (show es)
