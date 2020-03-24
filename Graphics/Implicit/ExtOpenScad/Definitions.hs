-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP, APTest, APBranch, APTerminator, APFailIf, APExample),
                                                  Symbol(Symbol),
                                                  Pattern(Wild, Name, ListP),
                                                  Expr(LitE, Var, ListE, LamE, (:$)),
                                                  StatementI(StatementI),
                                                  Statement(DoNothing, NewModule, Include, If, ModuleCall, (:=)),
                                                  OVal(ONum, OBool, OString, OList, OFunc, OUndefined, OUModule, ONModule, OVargsModule, OError, OObj2, OObj3),
                                                  TestInvariant(EulerCharacteristic),
                                                  SourcePosition(SourcePosition),
                                                  StateC,
                                                  CompState(CompState, scadVars, oVals, sourceDir, messages, scadOpts),
                                                  VarLookup(VarLookup),
                                                  Message(Message),
                                                  MessageType(TextOut, Warning, Error, SyntaxError, Compatibility, Unimplemented),
                                                  ScadOpts(ScadOpts, openScadCompatibility, importsAllowed),
                                                  lookupVarIn,
                                                  varUnion
                                                  ) where

import Prelude(Eq, Show, Ord, Maybe(Just), Bool(True, False), IO, FilePath, (==), show, ($), (<>), undefined, and, zipWith, foldl1, Int)

-- Resolution of the world, Integer type, and symbolic languages for 2D and 3D objects.
import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ, SymbolicObj2, SymbolicObj3, fromFastℕ)

import Control.Applicative (Applicative, Alternative((<|>), empty), pure, (<*>))

import Control.Monad (Functor, Monad, fmap, (>>=), mzero, mplus, MonadPlus, liftM, ap, return, (>=>))

import Data.Map (Map, lookup, union)

import Data.Maybe (fromMaybe)

import Data.Text.Lazy (Text, unpack, intercalate)

import Control.Monad.State (StateT)

-- | This is the state of a computation. It contains a hash of variables/functions, an array of OVals, a path, messages, and options controlling code execution.
data CompState = CompState
  { scadVars  :: VarLookup
  , oVals     :: [OVal]
  , sourceDir :: FilePath
  , messages  :: [Message]
  , scadOpts  :: ScadOpts
  }

type StateC = StateT CompState IO

-- | Handles parsing arguments to built-in modules
data ArgParser a
                 -- | For actual argument entries:
                 --   ArgParser (argument name) (default) (doc) (next Argparser...)
                 = AP Symbol (Maybe OVal) Text (OVal -> ArgParser a)
                 -- | For returns:
                 --   ArgParserTerminator (return value)
                 | APTerminator a
                 -- | For failure:
                 --   ArgParserFailIf (test) (error message) (child for if true)
                 | APFailIf Bool Text (ArgParser a)
                 --  An example, then next
                 | APExample Text (ArgParser a)
                 --  A string to run as a test, then invariants for the results, then next
                 | APTest Text [TestInvariant] (ArgParser a)
                 -- A branch where there are a number of possibilities for the parser underneath
                 | APBranch [ArgParser a]

instance Functor ArgParser where
    fmap = liftM

instance Applicative ArgParser where
    pure = APTerminator
    (<*>) = ap

instance Monad ArgParser where
    -- We need to describe how (>>=) works.
    -- Let's get the hard ones out of the way first.
    -- ArgParser actually
    (AP str fallback d f) >>= g = AP str fallback d (f >=> g)
    (APFailIf b errmsg child) >>= g = APFailIf b errmsg (child >>= g)
    -- These next two are easy, they just pass the work along to their child
    (APExample str child) >>= g = APExample str (child >>= g)
    (APTest str tests child) >>= g = APTest str tests (child >>= g)
    -- And an ArgParserTerminator happily gives away the value it contains
    (APTerminator a) >>= g = g a
    (APBranch bs) >>= g = APBranch $ fmap (>>= g) bs
    return = pure

instance MonadPlus ArgParser where
    mzero = APFailIf True "" undefined
    mplus (APBranch as) (APBranch bs) = APBranch ( as  <>  bs )
    mplus (APBranch as) b             = APBranch ( as  <> [b] )
    mplus a             (APBranch bs) = APBranch ( a   :   bs )
    mplus a             b             = APBranch [ a   ,   b  ]

instance Alternative ArgParser where
        (<|>) = mplus
        empty = mzero

newtype Symbol = Symbol Text
  deriving (Show, Eq, Ord)

newtype VarLookup = VarLookup (Map Symbol OVal)

data Pattern = Name Symbol
             | ListP [Pattern]
             | Wild
    deriving (Show, Eq)

-- | An expression.
data Expr = Var Symbol
          | LitE OVal -- A literal value.
          | ListE [Expr] -- A list of expressions.
          | LamE [Pattern] Expr -- A lambda expression.
          | Expr :$ [Expr] -- application of a function.
    deriving (Show, Eq)

-- | A statement, along with the line, column number, and file it is found at.
data StatementI = StatementI SourcePosition (Statement StatementI)
    deriving (Show, Eq)

data Statement st = Include Text Bool
               | Pattern :=  Expr
               | If Expr [st] [st]
               | NewModule  Symbol [(Symbol, Maybe Expr)] [st]
               | ModuleCall Symbol [(Maybe Symbol, Expr)] [st]
               | DoNothing
    deriving (Show, Eq)

-- | Objects for our OpenSCAD-like language
data OVal = OUndefined
         | OError [Text]
         | OBool Bool
         | ONum ℝ
         | OList [OVal]
         | OString Text
         | OFunc (OVal -> OVal)
         -- Name, arguments, argument parsers.
         | OUModule Symbol (Maybe [(Symbol, Bool)]) ([OVal] -> ArgParser (StateC [OVal]))
         -- Name, implementation, arguments, whether the module accepts/requires a suite.
         | ONModule Symbol (SourcePosition -> [OVal] -> ArgParser (StateC [OVal])) [([(Symbol, Bool)],  Maybe Bool)]
         | OVargsModule Symbol (Symbol -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ())
         | OObj3 SymbolicObj3
         | OObj2 SymbolicObj2

instance Eq OVal where
    (OBool a) == (OBool b) = a == b
    (ONum  a) == (ONum  b) = a == b
    (OList a) == (OList b) = and $ zipWith (==) a b
    (OString a) == (OString b) = a == b
    _ == _ = False

instance Show OVal where
    show OUndefined = "Undefined"
    show (OBool b) = show b
    show (ONum n) = show n
    show (OList l) = show l
    show (OString s) = show s
    show (OFunc _) = "<function>"
    show (OUModule (Symbol name) arguments _) = "module " <> (unpack name) <> " (" <> (unpack $ intercalate ", " (fmap showArg $ fromMaybe [] arguments)) <> ") {}"
      where
        showArg :: (Symbol, Bool) -> Text
        showArg (Symbol a, hasDefault) = if hasDefault
                                         then a
                                         else a <> "=..."
    show (ONModule (Symbol name) _ instances) = unpack $ showInstances instances
      where
        showArg (Symbol a, hasDefault) = if hasDefault
                                         then a
                                         else a <> "=..."
        showInstances :: [([(Symbol, Bool)], Maybe Bool)] -> Text
        showInstances [] = ""
        showInstances [oneInstance] = "module " <> name <> (showInstance oneInstance)
        showInstances multipleInstances = "Module " <> name <> "[ " <> (intercalate ", " (fmap showInstance multipleInstances)) <> " ]"
        showInstance :: ([(Symbol, Bool)], Maybe Bool) -> Text
        showInstance (arguments, suiteInfo) = " (" <> intercalate ", " (fmap showArg arguments) <> ") {}" <> showSuiteInfo suiteInfo
        showSuiteInfo :: Maybe Bool -> Text
        showSuiteInfo suiteInfo = case suiteInfo of
                          Just requiresSuite -> if requiresSuite
                                                then " requiring suite {}"
                                                else " accepting suite {}"
                          _ -> ""
    show (OVargsModule (Symbol name) _) = "varargs module " <> (unpack name)
    show (OError msgs) = unpack $ "Execution Error:\n" <> foldl1 (\a b -> a <> "\n" <> b) msgs
    show (OObj2 obj) = "<obj2: " <> show obj <> ">"
    show (OObj3 obj) = "<obj3: " <> show obj <> ">"

-- | In order to not propagate Parsec or other modules around, create our own source position type for the AST.
data SourcePosition = SourcePosition
    Fastℕ -- sourceLine
    Fastℕ -- sourceColumn
    FilePath -- sourceName
    deriving (Eq)

instance Show SourcePosition where
    show (SourcePosition line col []) = "line " <> show (fromFastℕ line :: Int) <> ", column " <> show (fromFastℕ col :: Int)
    show (SourcePosition line col filePath) = "line " <> show (fromFastℕ line :: Int) <> ", column " <> show (fromFastℕ col :: Int) <> ", file " <> filePath

-- | The types of messages the execution engine can send back to the application.
data MessageType = TextOut -- text intetionally output by the ExtOpenScad program.
                 | Warning
                 | Error
                 | SyntaxError
                 | Compatibility
                 | Unimplemented
  deriving (Show, Eq)

-- | An individual message.
data Message = Message MessageType SourcePosition Text
  deriving (Eq)

instance Show Message where
  show (Message mtype pos text) = show mtype <> " at " <> show pos <> ": " <> (unpack text)

-- | Options changing the behavior of the extended OpenScad engine.
data ScadOpts = ScadOpts
  { openScadCompatibility :: Bool
  , importsAllowed        :: Bool
  }

-- helper, to use union on VarLookups.
varUnion :: VarLookup -> VarLookup -> VarLookup
varUnion (VarLookup a) (VarLookup b) = VarLookup $ union a b

-- | For programs using this API to perform variable lookups, after execution of an escad has completed.
lookupVarIn :: Text -> VarLookup -> Maybe OVal
lookupVarIn target (VarLookup vars) = lookup (Symbol target) vars

newtype TestInvariant = EulerCharacteristic ℕ
    deriving (Show)
