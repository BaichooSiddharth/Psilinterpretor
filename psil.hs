-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-auteurs
  Siddharth Baichoo matricule 20130259
  Yi Cong Li matricule 20122756-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)
--
--
--
{-
(readSexp "(case (cons 1 2) ((nil) 1) ((cons x y) (+ x y)))")
Scons (Scons (Scons (Scons Snil (Ssym "case")) 
              (Scons (Scons (Scons Snil (Ssym "cons")) 
                            (Snum 1)) (Snum 2))) 
              (Scons (Scons Snil (Scons Snil (Ssym "nil"))) 
              (Snum 1))) 
              (Scons (Scons Snil (Scons (Scons (Scons Snil (Ssym "cons")) 
              (Ssym "x")) (Ssym "y"))) 
              (Scons (Scons (Scons Snil (Ssym "+")) 
              (Ssym "x")) (Ssym "y")))

readSexp "(case nil (nil 1) ((cons x y) (+ x y)))"
Scons (Scons (Scons (Scons Snil (Ssym "case"))
      (Ssym "nil")) 
      (Scons (Scons Snil (Ssym "nil")) 
      (Snum 1))) 
      (Scons (Scons Snil (Scons (Scons (Scons Snil (Ssym "cons"))
      (Ssym "x")) (Ssym "y"))) 
      (Scons (Scons (Scons Snil (Ssym "+")) 
      (Ssym "x")) (Ssym "y")))

readSexp "(let (((f x y) (* (+ x 1) y))))" =>
Scons (Scons Snil (Ssym "let")) 
      (Scons Snil (Scons 
      (Scons Snil (Scons 
      (Scons (Scons Snil (Ssym "f")) 
      (Ssym "x")) (Ssym "y"))) 
      (Scons (Scons (Scons Snil (Ssym "*")) (Scons (Scons (Scons Snil (Ssym "+")) 
      (Ssym "x")) (Snum 1))) 
      (Ssym "y"))))

readSexp "(f 3 5)"
Scons (Scons (Scons Snil (Ssym "f")) (Snum 3)) (Snum 5)

readSexp "((lambda (x) x) 2)"
Scons (Scons Snil 
      (Scons (Scons (Scons Snil (Ssym "lambda")) 
      (Scons Snil (Ssym "x"))) (Ssym "x"))) 
      (Snum 2)

readSexp "(cons nil)"
Scons (Scons Snil (Ssym "cons")) (Ssym "nil")

readSexp "(cons cons 1 2)"
Scons (Scons (Scons (Scons Snil (Ssym "cons")) (Ssym "cons")) (Snum 1)) (Snum 2)

readSexp "((lambda (x) x) 2)"
Scons (Scons Snil (Scons (Scons (Scons Snil (Ssym "lambda")) (Scons Snil (Ssym "x"))) (Ssym "x"))) (Snum 2)

readSexp "(((lambda (x) (lambda (y) (* x y)))3)5)"
Scons (Scons Snil (Scons (Scons Snil (Scons (Scons (Scons Snil (Ssym "lambda")) 
                        (Scons Snil (Ssym "x"))) 
                        (Scons (Scons (Scons Snil (Ssym "lambda")) 
                        (Scons Snil (Ssym "y"))) 
                        (Scons (Scons (Scons Snil (Ssym "*")) (Ssym "x")) (Ssym "y"))))) 
                        (Snum 3))) (Snum 5)
                                              -}

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un numbre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote = do { char '\''; pSpaces; e <- pSexp;
              return (Scons (Scons Snil (Ssym "quote")) e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do char '('
           pSpaces
           (do { char ')'; return Snil }
            <|> do head <- (do e <- pSexp
                               pSpaces
                               (do char '.'
                                   pSpaces
                                   return e
                                <|> return (Scons Snil e)))
                   pLiat head)
    where pLiat :: Sexp -> Parser Sexp
          pLiat head = do char ')'
                          return head
                   <|> do e <- pSexp
                          pSpaces
                          pLiat (Scons head e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e2) = showString "(" . showSexp' e2
          showHead (Scons e1 e2) = showHead e1 . showString " " . showSexp' e2
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lfun Var Lexp       -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lnil                -- "Constructeur" de liste vide.
          | Lcons Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lletrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)

--Simplifie certains cas de Lexp
filterer:: Lexp->Lexp
filterer (Lcall (Lcall Lnil Lnil) a) = a
filterer (a) = a

--fonction helper pour aider s2l (Pas sur si nécessaire)
helper :: Lexp -> Lexp -> Lexp
helper (Lcall Lnil Lnil) x = x
helper (Lcall Lnil x) y = Lcall x y
helper (Lcall x y) z = Lcall (Lcall x y) z
--helper (Lcons Lnil x) y = Lcons x y 
--helper (Lcons x y) z = Lcons (Lcons x y) z
helper (Lfun x y) z  = Lcall (Lfun x y) z
helper a b = Lcall a b

--fonction pour "purifier" les Lcons d'expression pas pertinants
purifyCons :: Lexp -> Lexp
purifyCons (Lcons (Lcall a (Lvar "cons")) (Lcall b c)) =
  Lcons (purifyCons a) (Lcons (purifyCons b) (purifyCons c))
purifyCons (Lcons (Lcons a (Lvar "cons")) b) = Lcons (purifyCons a) (purifyCons b)
purifyCons (Lcall a b) = Lcons (purifyCons a) (purifyCons b) 
purifyCons (Lcall (a) (Lvar "cons")) = Lcons Lnil (purifyCons a)
purifyCons (Lcall Lnil (a)) = purifyCons a
purifyCons (Lcons (Lcons Lnil a) b) = Lcons (purifyCons a) (purifyCons b)
purifyCons (Lcons a b) = Lcons (purifyCons a) (purifyCons b)
purifyCons (Lcons (Lcons a Lnil) b) = Lcons (purifyCons a) (purifyCons b)
purifyCons (Lnum a) = Lnum a
purifyCons (Lnil) = Lnil
purifyCons (Lvar a)
  | a /= "cons" = Lvar a
  | a == "cons" = Lnil
purifyCons a = error ("Malformed Lexp: " ++ show a)

--trouve des Var
findVar :: Sexp -> Var
findVar (Scons (Snil) (Ssym a)) = a
findVar se = error ("Malformed Sexp: " ++ (showSexp se))

--fonction qui est utilisée pour construire les Case
findOthers :: Sexp -> (Var, Var, Lexp)
findOthers (Scons (Scons Snil (Scons (Scons (Scons Snil (Ssym "cons")) 
  (Ssym a)) (Ssym b))) c) = (a,b, s2l c)
findOthers se = error ("Malformed Sexp: " ++ (showSexp se))

getfirst :: (Var,Var,Lexp) -> Var 
getfirst(a,_,_) = a

getSecond :: (Var, Var, Lexp) -> Var
getSecond(_,a,_) = a

getThird :: (Var, Var, Lexp) -> Lexp
getThird(_,_,a) = a

--obtient les arguments des Let
getArguments :: Sexp-> [Var]
getArguments(Scons Snil (Ssym a)) = [a]
getArguments(Snil) = []
getArguments(Ssym a) = [a]
getArguments(Scons a b) = (getArguments a) ++ (getArguments b)
getArguments se = error ("Malformed Sexp: " ++ (showSexp se))

--construit le lexp qui correspnd a une variable
constLexp :: [Var] -> Sexp -> Lexp
constLexp [](l) = (s2l l)
constLexp xs(l)
  |xs!!0 == "lambda" = constLexp (drop 1 xs) (l)
  |xs!!0 == "case"   = error "Vous ne pouvez pas redefinir un case"
  |otherwise   = Lfun (xs!!0) (constLexp (drop 1 xs) (l))

--construit un tuple
constTuple :: [Var] -> Sexp -> (Var, Lexp)
constTuple [](l) = error "pas d'arguments"
constTuple xs(l) = (xs!!0, constLexp(drop 1 xs)(l))

--construit la liste de tuples
constList :: Sexp -> [(Var, Lexp)]
constList (Scons Snil (Scons (Scons Snil a) (b) )) = 
  [constTuple (getArguments a) b]
constList (Scons (Scons Snil (Scons a b)) (Scons (Scons Snil c) d)) = 
  [(constTuple (getArguments a) b)] ++ [constTuple (getArguments c) d]
constList (Scons (Scons Snil a) (b)) = [constTuple (getArguments a) b]
constList (Scons a b) = (constList a) ++ (constList b)  
constList se = error ("Malformed Sexp: " ++ (showSexp se))

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Snil) = Lnil
s2l (Ssym "nil") = Lnil 
s2l (Ssym s) = Lvar s
--s2l (Scons (Scons a (Ssym "cons")) (Scons b c)) = Lcons (s2l a) (Lcons (s2l b) (s2l b)) 
--s2l (Scons (Scons (Scons (Scons Snil (Ssym "cons")) a) (Ssym "cons")) (Scons (Scons Snil b) c)) =
  --Lcons (s2l a) (Lcons (s2l b) (s2l c)) 
s2l (Scons Snil (Scons (Scons (Scons Snil (Ssym "lambda")) a) b)) = 
  Lfun (findVar a) (s2l b)
--cas pour les fonctions anonymes
s2l (Scons (Scons (Scons Snil (Ssym "lambda")) a) b) = Lfun (findVar a) (s2l b)
--cas pour les Lcall
s2l (Scons (Scons Snil (Ssym a)) (b))
  | a/="cons" && a/= "case" && a/= "lambda" && a/="nil" = Lcall (Lvar a) (s2l b)
  | a=="nil"                                            = s2l(b)
  | otherwise                                           = Lnil
--cas pour les Lcons
s2l (Scons (Scons Snil (Ssym "cons")) a) = 
  purifyCons(Lcons Lnil (purifyCons(s2l a)))
s2l (Scons (Scons (Scons Snil (Ssym "cons")) a) b) = 
  purifyCons(Lcons (purifyCons(s2l a)) (purifyCons(s2l b)))
s2l (Scons (Scons (Scons (Scons Snil (Ssym "cons")) a) (Ssym "cons")) b) = 
  purifyCons(Lcons (purifyCons(s2l a)) (purifyCons(s2l b)))
s2l (Scons (Scons (Scons Snil a) (Ssym "cons")) b) = 
  purifyCons(Lcons (purifyCons(s2l a)) (purifyCons(s2l b)))
--cas pour le case
s2l(Scons (Scons (Scons (Scons Snil (Ssym "case")) a) b) c) = 
  Lcase (s2l a) (filterer(s2l b)) c' d' e'
  where tuple = findOthers c
        c' = getfirst tuple
        d' = getSecond tuple
        e' = getThird tuple
--cas pour le let
s2l (Scons(Scons (Scons Snil (Ssym "let")) b) c)= Lletrec (constList b) (s2l c)

s2l (Scons a b)= helper a' b'  
       where a' = s2l a
             b' = s2l b

-- ¡¡ COMPLETER !!
s2l se = error ("Malformed Sexp: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1 v2) =
                showChar ' ' . showsPrec p v1 . showTail v2
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec p _ = showString "<function>"

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
-- ATTENTION: il est important de l'utiliser autant pour `l2d` que pour `eval`!
env0 :: Env
env0 = [("+", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))),
        ("*", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))),
        ("/", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))),
        ("-", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y))))]

---------------------------------------------------------------------------
-- Représentation intermédiaire Dexp                  
                   --
---------------------------------------------------------------------------

-- Dexp est similaire à Lexp sauf que les variables sont représentées non
-- pas par des chaînes de caractères mais par des "Indexes de de Bruijn",
-- c'est à dire par leur distance dans l'environnment: la variable la plus
-- récemment déclarée a index 0, l'antérieure 1, etc...
--
-- I.e. Lfun "x" (Lfun "y" (Lcons (Lvar "x") (Lvar "y")))
-- se traduit par Dfun (Dfun (Dcons (Dvar 1) (Dvar 0)))

type Idx = Int

data Dexp = Dnum Int            -- Constante entière.
          | Dvar Idx            -- Référence à une variable de l'environnement.
          | Dfun Dexp           -- Fonction anonyme prenant un argument.
          | Dcall Dexp Dexp     -- Appel de fonction, avec un argument.
          | Dnil                -- Constructeur de liste vide.
          | Dcons Dexp Dexp     -- Constructeur de liste.
          | Dcase Dexp Dexp Dexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dletrec [Dexp] Dexp
          deriving (Show, Eq)

--environnement de variables associées aux index de Bruijn pour manipuler les variables avec leur position relative au lieu de "String", éviter la mauvaise capture par nom
type Lenv = [(Var, Idx)]

-- env initial
env1 :: Lenv
env1 = [("+", 0),
        ("*", 1),
        ("/", 2),
        ("-", 3),
        ("list", 4)]

--incrémenter les index lors d'ajout d'une nouvelle variable
env_increment ::Lenv -> Lenv
env_increment (x:xs)
  |xs == [] = [(fst x, (snd x)+1)]
  |otherwise = ((fst x, (snd x)+1):(env_increment xs))

env_add ::Lenv -> Lexp -> Lenv
--ajout des variables à environnement dans différents cas
env_add (x:xs) (Lfun v exp) = ((v, (snd x)-1):(x:xs))

env_add (x:xs) (Lcase e1 e2 v1 v2 e3) = ((v2, (snd x)-2):((v1, (snd x)-1):(x:xs)))

env_add (x:xs) (Lletrec (z:zs) exp)
  |zs == [] = env_increment(((fst z, (snd x)-1):(x:xs)))
  |otherwise = env_increment(env_add ((fst z, (snd x)-1):(x:xs)) (Lletrec zs exp))

--l'interpréteur pour différents constructeurs de Lexp qui prend en compte de l'environnement actuel
l2d' :: Lenv -> Lexp -> Dexp
l2d' _ (Lnum n) = Dnum n
l2d' lenv (Lvar v)
  |i  == -1 = error ("variable not defined in environment: " ++ show v)
  |otherwise = Dvar i
  where i = (env_lookup lenv v)
l2d' lenv (Lfun v exp) = Dfun (l2d' lenv' exp) where lenv' = env_increment(env_add lenv (Lfun v exp))
l2d' lenv (Lcall e1 e2) = Dcall (l2d' lenv e1) (l2d' lenv e2)
l2d' lenv (Lnil) = Dnil
l2d' lenv (Lcons e1 e2) = Dcons (l2d' lenv e1) (l2d' lenv e2)
l2d' lenv (Lcase e en v1 v2 ec) = 
  Dcase (l2d' lenv' e) (l2d' lenv' en) (l2d' lenv' ec)
  where lenv' = env_increment(env_increment(env_add lenv (Lcase e en v1 v2 ec)))
l2d' lenv (Lletrec (y:ys) exp) = 
  Dletrec (map (l2d' lenv') (map snd (y:ys))) (l2d' lenv' exp)
  where lenv' = env_add lenv (Lletrec (y:ys) exp) 

--chercher une variable selon son index dans l'environnement
env_lookup :: Lenv -> Var -> Idx
env_lookup (x:xs) var
  |var == (fst x) = (snd x)
  |xs == [] = -1
  |otherwise = env_lookup xs var

--l'interpréteur top-level, qui commence l'interprétation avec env initial
l2d :: Lexp -> Dexp
l2d (Lnum n) = l2d' env1 (Lnum n)
l2d (Lvar v) = l2d' env1 (Lvar v)
l2d (Lfun v exp) = l2d' env1 (Lfun v exp)
l2d (Lcall e1 e2) = l2d' env1 (Lcall e1 e2)
l2d (Lnil) = l2d' env1 (Lnil)
l2d (Lcons e1 e2) = l2d' env1 (Lcons e1 e2)
l2d (Lcase e en v1 v2 ec) = l2d' env1 (Lcase e en v1 v2 ec)
l2d (Lletrec (y:ys) exp) = l2d' env1 (Lletrec (y:ys) exp)

l2d e = error ("Unhandled l2d: " ++ show e)

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------


--environnement d'index et Value, pour manipuler la valeur associée à chaque index correspondant et l'évaluer
type Denv = [(Idx, Value)]

--env initial
env2 :: Denv
env2 = [(0, Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))),
        (1, Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))),
        (2, Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))),
        (3, Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y)))),
        (4, Vfun (\ x -> Vcons x Vnil))]--à compléter pour list

--incrémenter les index lors d'ajout d'une nouvelle valeur
denv_increment ::Denv -> Denv
denv_increment (x:xs)
  |length xs == 0 = [((fst x)+1, snd x)]
  |otherwise = (((fst x)+1, snd x):(denv_increment xs))

--ajout des valeurs à environnement dans différents cas
denv_add ::Denv -> Dexp -> Denv
denv_add (x:xs) (Dcall (Dfun e1) e2) = denv_increment((((fst x)-1, eval' (x:xs) e2):(x:xs)))
denv_add (x:xs) (Dcall e1 e2) = (x:xs)

denv_add (x:xs) (Dcase e1 e2 e3) = 
  case eval' (x:xs) e1 of
    Vcons a b -> denv_increment(denv_increment((((fst x)-2, a):(((fst x)-1, b) :(x:xs)))))

denv_add (x:xs) (a) = denv_increment(((fst x)-1, (eval' (x:xs) a)):(x:xs))

denv_add (x:xs) (Dletrec (z:zs) exp)
  |zs == [] = denv_increment(((fst x)-1, (eval' (x:xs) z)):(x:xs))
  |otherwise = denv_increment(denv_add ((((fst x)-1, (eval' (x:xs) z)):(x:xs))) (Dletrec zs exp))

--l'évaluateur pour différents constructeurs de Dexp qui prend en compte de l'environnement actuel
eval' :: Denv -> Dexp -> Value
eval' _ (Dnum n) = Vnum n
eval' denv (Dvar i) = denv_lookup denv i

eval' denv (Dcall(Dcall (Dcall (Dfun (Dfun (Dfun e1)))e2)e3)e4) = 
  eval' denv''' e1
  where
    denv''' = denv_add denv'' (Dcall (Dfun e1) e2)
    denv'' = denv_add denv' (Dcall (Dfun e1) e3)
    denv' = denv_add denv (Dcall (Dfun e1) e4)

eval' denv (Dcall (Dcall (Dfun (Dfun e1)) e2) e3 ) = eval' denv'' e1
  where
    denv'' = denv_add denv' (Dcall (Dfun e1) e2)
    denv' = denv_add denv (Dcall (Dfun e1) e3)

eval' denv (Dcall (Dfun e1) e2) = eval' denv' e1 where denv' = denv_add denv (Dcall (Dfun e1) e2)

eval' denv (Dcall e1 e2) = case eval' denv e1 of
                             Vfun body -> body(eval' denv e2) 
eval' denv (Dnil) = Vnil
eval' denv (Dcons e1 e2) = Vcons (eval' denv e1) (eval' denv e2)
eval' denv (Dcase e en ec) = case (eval' denv e) of
                               Vnil -> eval' denv en
                               Vcons a b -> eval' denv' ec
                               where denv' = denv_add denv (Dcase e en ec)

eval' denv (Dletrec (y:ys) exp) = eval' denv' exp
  where denv' = denv_add denv (Dletrec (y:ys) exp) 

--chercher une valeur selon son index dans l'environnement
denv_lookup :: Denv -> Idx -> Value
denv_lookup (x:xs) i
  |i == (fst x) = (snd x)
  |length xs == 0 = error ("variable not defined in environment: " ++ show i)
  |otherwise = denv_lookup xs i

--l'évaluateur top-level, qui commence l'évaluation avec env initial
eval :: Dexp -> Value
eval (Dnum n) = eval' env2 (Dnum n)
eval (Dvar i) = eval' env2 (Dvar i)
eval (Dfun exp) = eval' env2 (Dfun exp)
eval (Dcall e1 e2) = eval' env2 (Dcall e1 e2)
eval (Dnil) = eval' env2 (Dnil)
eval (Dcons e1 e2) = eval' env2 (Dcons e1 e2)
eval (Dcase e en ec) = eval' env2 (Dcase e en ec)
eval (Dletrec (x:xs) exp) = eval' env2 (Dletrec (x:xs) exp)

eval e = error ("Unhandled eval: " ++ show e)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval . l2d . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in map evalSexp (sexps s))
