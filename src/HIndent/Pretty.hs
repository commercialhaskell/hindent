{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Pretty printing.

module HIndent.Pretty
  (pretty)
  where

import           Control.Applicative
import           Control.Monad.State.Strict hiding (state)
import qualified Data.ByteString.Builder as S
import           Data.Foldable (for_, traverse_)
import           Data.Int
import           Data.List
import           Data.Monoid ((<>))
import           Data.Maybe
import           Data.Typeable
import           HIndent.Types
import qualified Language.Haskell.Exts as P
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

--------------------------------------------------------------------------------
-- * Pretty printing class

-- | Pretty printing class.
class (Annotated ast,Typeable ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()

-- | Pretty print including comments.
pretty :: (Pretty ast,Show (ast NodeInfo))
       => ast NodeInfo -> Printer ()
pretty a = do
  mapM_
    (\c' -> do
       case c' of
         CommentBeforeLine _ c -> do
           case c of
             EndOfLine s -> write ("--" ++ s)
             MultiLine s -> write ("{-" ++ s ++ "-}")
           newline
         _ -> return ())
    comments
  prettyInternal a
  mapM_
    (\(i, c') -> do
       case c' of
         CommentSameLine spn c -> do
           col <- gets psColumn
           if col == 0
             then do
               -- write comment keeping original indentation
               let col' = fromIntegral $ srcSpanStartColumn spn - 1
               column col' $ writeComment c
             else do
               space
               writeComment c
         CommentAfterLine spn c -> do
           when (i == 0) newline
           -- write comment keeping original indentation
           let col = fromIntegral $ srcSpanStartColumn spn - 1
           column col $ writeComment c
         _ -> return ())
    (zip [0 :: Int ..] comments)
  where
    comments = nodeInfoComments (ann a)
    writeComment =
      \case
        EndOfLine cs -> do
          write ("--" ++ cs)
          modify
            (\s ->
                s
                { psEolComment = True
                })
        MultiLine cs -> do
          write ("{-" ++ cs ++ "-}")
          modify
            (\s ->
                s
                { psEolComment = True
                })

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' :: (Pretty ast,P.Pretty (ast SrcSpanInfo))
        => ast NodeInfo -> Printer ()
pretty' = write . P.prettyPrint . fmap nodeInfoSpan

--------------------------------------------------------------------------------
-- * Combinators

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = level + i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

indentedBlock :: Printer a -> Printer a
indentedBlock p =
  do indentSpaces <- getIndentSpaces
     indented indentSpaces p

-- | Print all the printers separated by spaces.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

-- | Print all the printers separated by commas.
commas :: [Printer ()] -> Printer ()
commas = inter (write ", ")

-- | Print all the printers separated by sep.
inter :: Printer () -> [Printer ()] -> Printer ()
inter sep ps =
  foldr
    (\(i,p) next ->
        depend
          (do p
              if i < length ps
                then sep
                else return ())
          next)
    (return ())
    (zip [1 ..] ps)

-- | Print all the printers separated by newlines.
lined :: [Printer ()] -> Printer ()
lined ps = sequence_ (intersperse newline ps)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined pref ps' =
  case ps' of
    [] -> return ()
    (p:ps) ->
      do p
         indented (fromIntegral
                     (length pref *
                      (-1)))
                  (mapM_ (\p' ->
                            do newline
                               depend (write pref) p')
                         ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Output a newline.
newline :: Printer ()
newline =
  do write "\n"
     modify (\s -> s {psNewline = True})

-- | Set the context to a case context, where RHS is printed with -> .
withCaseContext :: Bool -> Printer a -> Printer a
withCaseContext bool pr =
  do original <- gets psInsideCase
     modify (\s -> s {psInsideCase = bool})
     result <- pr
     modify (\s -> s {psInsideCase = original})
     return result

-- | Get the current RHS separator, either = or -> .
rhsSeparator :: Printer ()
rhsSeparator =
  do inCase <- gets psInsideCase
     if inCase
        then write "->"
        else write "="

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do state' <- get
     maker
     st <- get
     col <- gets psColumn
     if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col dependent
        else dependent

-- | Wrap.
wrap :: String -> String -> Printer a -> Printer a
wrap open close p = depend (write open) $ p <* write close

-- | Wrap in parens.
parens :: Printer a -> Printer a
parens = wrap "(" ")"

-- | Wrap in braces.
braces :: Printer a -> Printer a
braces = wrap "{" "}"

-- | Wrap in brackets.
brackets :: Printer a -> Printer a
brackets = wrap "[" "]"

-- | Write a space.
space :: Printer ()
space = write " "

-- | Write a comma.
comma :: Printer ()
comma = write ","

-- | Write an integral.
int :: Integer -> Printer ()
int = write . show

-- | Write out a string, updating the current position information.
write :: String -> Printer ()
write x =
  do eol <- gets psEolComment
     hardFail <- gets psFitOnOneLine
     let addingNewline = eol && x /= "\n"
     when addingNewline newline
     state <- get
     let writingNewline = x == "\n"
         out :: String
         out =
           if psNewline state && not writingNewline
              then (replicate (fromIntegral (psIndentLevel state))
                               ' ') <>
                   x
              else x
         psColumn' =
            if additionalLines > 0
               then fromIntegral (length (concat (take 1 (reverse srclines))))
               else psColumn state + fromIntegral (length out)
     when
       hardFail
       (guard
          (additionalLines == 0 &&
           (psColumn' <= configMaxColumns (psConfig state))))
     modify (\s ->
               s {psOutput = psOutput state <> S.stringUtf8 out
                 ,psNewline = False
                 ,psLine = psLine state + fromIntegral additionalLines
                 ,psEolComment= False
                 ,psColumn = psColumn'})
  where srclines = lines x
        additionalLines =
          length (filter (== '\n') x)

-- | Write a string.
string :: String -> Printer ()
string = write

-- | Indent spaces, e.g. 2.
getIndentSpaces :: Printer Int64
getIndentSpaces =
  gets (configIndentSpaces . psConfig)

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: Printer a -> Printer (a,PrintState)
sandbox p =
  do orig <- get
     a <- p
     new <- get
     put orig
     return (a,new)

-- | Render a type with a context, or not.
withCtx :: (Pretty ast,Show (ast NodeInfo))
        => Maybe (ast NodeInfo) -> Printer b -> Printer b
withCtx Nothing m = m
withCtx (Just ctx) m =
  do pretty ctx
     write " =>"
     newline
     m

-- | Maybe render an overlap definition.
maybeOverlap ::  Maybe (Overlap NodeInfo) -> Printer ()
maybeOverlap =
  maybe (return ())
        (\p ->
           pretty p >>
           space)

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer () -> Printer b -> Printer ()
swing a b =
  do orig <- gets psIndentLevel
     a
     mst <- fitsOnOneLine (do space
                              b)
     case mst of
       Just st -> put st
       Nothing -> do newline
                     indentSpaces <- getIndentSpaces
                     _ <- column (orig + indentSpaces) b
                     return ()

-- | Swing the second printer below and indented with respect to the first by
-- the specified amount.
swingBy :: Int64 -> Printer() -> Printer b -> Printer b
swingBy i a b =
  do orig <- gets psIndentLevel
     a
     newline
     column (orig + i) b

--------------------------------------------------------------------------------
-- * Instances

instance Pretty Context where
  prettyInternal ctx@(CxTuple _ asserts) = do
    mst <- fitsOnOneLine (parens (inter (comma >> space) (map pretty asserts)))
    case mst of
      Nothing -> context ctx
      Just st -> put st
  prettyInternal ctx = context ctx

instance Pretty Pat where
  prettyInternal x =
    case x of
      PLit _ sign l -> pretty sign >> pretty l
      PNPlusK _ n k ->
        depend (do pretty n
                   write "+")
               (int k)
      PInfixApp _ a op b ->
        case op of
          Special{} ->
            depend (pretty a)
                   (depend (prettyInfixOp op)
                           (pretty b))
          _ ->
            depend (do pretty a
                       space)
                   (depend (do prettyInfixOp op
                               space)
                           (pretty b))
      PApp _ f args ->
        depend (do pretty f
                   unless (null args) space)
               (spaced (map pretty args))
      PTuple _ boxed pats ->
        depend (write (case boxed of
                         Unboxed -> "(# "
                         Boxed -> "("))
               (do commas (map pretty pats)
                   write (case boxed of
                            Unboxed -> " #)"
                            Boxed -> ")"))
      PList _ ps ->
        brackets (commas (map pretty ps))
      PParen _ e -> parens (pretty e)
      PRec _ qname fields -> do
        let horVariant = do
              pretty qname
              space
              braces $ commas $ map pretty fields
            verVariant =
              depend (pretty qname >> space) $ do
                case fields of
                  [] -> write "{}"
                  [field] -> braces $ pretty field
                  _ -> do
                    depend (write "{") $
                      prefixedLined "," $ map (depend space . pretty) fields
                    newline
                    write "}"
        horVariant `ifFitsOnOneLineOrElse` verVariant
      PAsPat _ n p ->
        depend (do pretty n
                   write "@")
               (pretty p)
      PWildCard _ -> write "_"
      PIrrPat _ p ->
        depend (write "~")
               (pretty p)
      PatTypeSig _ p ty ->
        depend (do pretty p
                   write " :: ")
               (pretty ty)
      PViewPat _ e p ->
        depend (do pretty e
                   write " -> ")
               (pretty p)
      PQuasiQuote _ name str -> quotation name (string str)
      PBangPat _ p ->
        depend (write "!")
               (pretty p)
      PRPat{} -> pretty' x
      PXTag{} -> pretty' x
      PXETag{} -> pretty' x
      PXPcdata{} -> pretty' x
      PXPatTag{} -> pretty' x
      PXRPats{} -> pretty' x
      PVar{} -> pretty' x
      PSplice _ s -> pretty s
#if MIN_VERSION_haskell_src_exts(1,20,0)
      (PUnboxedSum _ nLeft nRight p) -> unboxedSumValuePattern nLeft nRight p
#endif

-- | Pretty infix application of a name (identifier or symbol).
prettyInfixName :: Name NodeInfo -> Printer ()
prettyInfixName (Ident _ n) = do write "`"; string n; write "`";
prettyInfixName (Symbol _ s) = string s

-- | Pretty print a name for being an infix operator.
prettyInfixOp ::  QName NodeInfo -> Printer ()
prettyInfixOp x =
  case x of
    Qual _ mn n ->
      case n of
        Ident _ i -> do write "`"; pretty mn; write "."; string i; write "`";
        Symbol _ s -> do pretty mn; write "."; string s;
    UnQual _ n -> prettyInfixName n
    Special _ s -> pretty s

prettyQuoteName :: Name NodeInfo -> Printer ()
prettyQuoteName x =
  case x of
    Ident _ i -> string i
    Symbol _ s -> string ("(" ++ s ++ ")")

instance Pretty Type where
  prettyInternal = typ

instance Pretty Exp where
  prettyInternal = exp

-- | Render an expression.
exp :: Exp NodeInfo -> Printer ()
-- | Do after lambda should swing.
exp (Lambda _ pats (Do l stmts)) =
  do
     mst <-
          fitsOnOneLine
            (do write "\\"
                spaced (map pretty pats)
                write " -> "
                pretty (Do l stmts))
     case mst of
       Nothing -> swing (do write "\\"
                            spaced (map pretty pats)
                            write " -> do")
                         (lined (map pretty stmts))
       Just st -> put st
-- | Space out tuples.
exp (Tuple _ boxed exps) = do
  let horVariant = parensHorB boxed $ inter (write ", ") (map pretty exps)
      verVariant = parensVerB boxed $ prefixedLined "," (map (depend space . pretty) exps)
  mst <- fitsOnOneLine horVariant
  case mst of
    Nothing -> verVariant
    Just st -> put st
  where
    parensHorB Boxed = parens
    parensHorB Unboxed = wrap "(# " " #)"
    parensVerB Boxed = parens
    parensVerB Unboxed = wrap "(#" "#)"
-- | Space out tuples.
exp (TupleSection _ boxed mexps) = do
  let horVariant = parensHorB boxed $ inter (write ", ") (map (maybe (return ()) pretty) mexps)
      verVariant =
        parensVerB boxed $ prefixedLined "," (map (maybe (return ()) (depend space . pretty)) mexps)
  mst <- fitsOnOneLine horVariant
  case mst of
    Nothing -> verVariant
    Just st -> put st
  where
    parensHorB Boxed = parens
    parensHorB Unboxed = wrap "(# " " #)"
    parensVerB Boxed = parens
    parensVerB Unboxed = wrap "(#" "#)"
#if MIN_VERSION_haskell_src_exts(1,20,0)
exp (UnboxedSum _ nLeft nRight e) = unboxedSumValuePattern nLeft nRight e
#endif
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp e@(InfixApp _ a op b) =
  infixApp e a op b Nothing
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp (If _ if' then' else') =
  do depend (write "if ")
            (pretty if')
     newline
     indentSpaces <- getIndentSpaces
     indented indentSpaces
              (do branch "then " then'
                  newline
                  branch "else " else')
     -- Special handling for do.
  where branch str e =
          case e of
            Do _ stmts ->
              do write str
                 write "do"
                 newline
                 indentSpaces <- getIndentSpaces
                 indented indentSpaces (lined (map pretty stmts))
            _ ->
              depend (write str)
                     (pretty e)
-- | Render on one line, or otherwise render the op with the arguments
-- listed line by line.
exp (App _ op arg) = do
  let flattened = flatten op ++ [arg]
  mst <- fitsOnOneLine (spaced (map pretty flattened))
  case mst of
    Nothing -> do
      let (f:args) = flattened
      col <- gets psColumn
      spaces <- getIndentSpaces
      pretty f
      col' <- gets psColumn
      let diff = col' - col - if col == 0 then spaces else 0
      if diff + 1 <= spaces
        then space
        else newline
      spaces' <- getIndentSpaces
      indented spaces' (lined (map pretty args))
    Just st -> put st
  where
    flatten (App label' op' arg') = flatten op' ++ [amap (addComments label') arg']
    flatten x = [x]
    addComments n1 n2 =
      n2
      { nodeInfoComments = nub (nodeInfoComments n2 ++ nodeInfoComments n1)
      }
-- | Space out commas in list.
exp (List _ es) =
  do mst <- fitsOnOneLine p
     case mst of
       Nothing -> do
         depend
           (write "[")
           (prefixedLined "," (map (depend space . pretty) es))
         newline
         write "]"
       Just st -> put st
  where p =
          brackets (inter (write ", ")
                          (map pretty es))
exp (RecUpdate _ exp' updates) = recUpdateExpr (pretty exp') updates
exp (RecConstr _ qname updates) = recUpdateExpr (pretty qname) updates
exp (Let _ binds e) =
  depend (write "let ")
         (do pretty binds
             newline
             indented (-3) (depend (write "in ")
                                   (pretty e)))
exp (ListComp _ e qstmt) = do
  let horVariant = brackets $ do
        pretty e
        write " | "
        commas $ map pretty qstmt
      verVariant = do
        write "[ "
        pretty e
        newline
        depend (write "| ") $ prefixedLined ", " $ map pretty qstmt
        newline
        write "]"
  horVariant `ifFitsOnOneLineOrElse` verVariant

exp (ParComp _ e qstmts) = do
  let horVariant = brackets $ do
        pretty e
        for_ qstmts $ \qstmt -> do
          write " | "
          commas $ map pretty qstmt
      verVariant = do
        depend (write "[ ") $ pretty e
        newline
        for_ qstmts $ \qstmt -> do
          depend (write "| ") $ prefixedLined ", " $ map pretty qstmt
          newline
        write "]"
  horVariant `ifFitsOnOneLineOrElse` verVariant

exp (TypeApp _ t) = do
  write "@"
  pretty t
exp (NegApp _ e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) = do
  write "\\"
  spaced [ do case (i, x) of
                (0, PIrrPat {}) -> space
                (0, PBangPat {}) -> space
                _ -> return ()
              pretty x
         | (i, x) <- zip [0 :: Int ..] ps
         ]
  swing (write " ->") $ pretty e
exp (Paren _ e) = parens (pretty e)
exp (Case _ e alts) =
  do depend (write "case ")
            (do pretty e
                write " of")
     if null alts
       then write " {}"
       else do newline
               indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (Do _ stmts) =
  depend (write "do ")
         (lined (map pretty stmts))
exp (MDo _ stmts) =
  depend (write "mdo ")
         (lined (map pretty stmts))
exp (LeftSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RightSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (EnumFrom _ e) =
  brackets (do pretty e
               write " ..")
exp (EnumFromTo _ e f) =
  brackets (depend (do pretty e
                       write " .. ")
                   (pretty f))
exp (EnumFromThen _ e t) =
  brackets (depend (do pretty e
                       write ",")
                   (do pretty t
                       write " .."))
exp (EnumFromThenTo _ e t f) =
  brackets (depend (do pretty e
                       write ",")
                   (depend (do pretty t
                               write " .. ")
                           (pretty f)))
exp (ExpTypeSig _ e t) =
  depend (do pretty e
             write " :: ")
         (pretty t)
exp (VarQuote _ x) =
  depend (write "'")
         (pretty x)
exp (TypQuote _ x) =
  depend (write "''")
         (pretty x)
exp (BracketExp _ b) = pretty b
exp (SpliceExp _ s) = pretty s
exp (QuasiQuote _ n s) = quotation n (string s)
exp (LCase _ alts) =
  do write "\\case"
     if null alts
       then write " {}"
       else do newline
               indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (MultiIf _ alts) =
  withCaseContext
    True
    (depend
       (write "if ")
       (lined
          (map
             (\p -> do
                write "| "
                prettyG p)
             alts)))
  where
    prettyG (GuardedRhs _ stmts e) = do
      indented
        1
        (do (lined (map
                         (\(i,p) -> do
                            unless (i == 1)
                                   space
                            pretty p
                            unless (i == length stmts)
                                   (write ","))
                         (zip [1..] stmts))))
      swing (write " " >> rhsSeparator) (pretty e)
exp (Lit _ lit) = prettyInternal lit
exp (Var _ q) = pretty q
exp (IPVar _ q) = pretty q
exp (Con _ q) = pretty q

exp x@XTag{} = pretty' x
exp x@XETag{} = pretty' x
exp x@XPcdata{} = pretty' x
exp x@XExpTag{} = pretty' x
exp x@XChildTag{} = pretty' x
exp x@CorePragma{} = pretty' x
exp x@SCCPragma{} = pretty' x
exp x@GenPragma{} = pretty' x
exp x@Proc{} = pretty' x
exp x@LeftArrApp{} = pretty' x
exp x@RightArrApp{} = pretty' x
exp x@LeftArrHighApp{} = pretty' x
exp x@RightArrHighApp{} = pretty' x
exp x@ParArray{} = pretty' x
exp x@ParArrayFromTo{} = pretty' x
exp x@ParArrayFromThenTo{} = pretty' x
exp x@ParArrayComp{} = pretty' x
exp (OverloadedLabel _ label) = string ('#' : label)

instance Pretty IPName where
 prettyInternal = pretty'

instance Pretty Stmt where
  prettyInternal =
    stmt

instance Pretty QualStmt where
  prettyInternal x =
    case x of
      QualStmt _ s -> pretty s
      ThenTrans _ s -> do
        write "then "
        pretty s
      ThenBy _ s t -> do
        write "then "
        pretty s
        write " by "
        pretty t
      GroupBy _ s -> do
        write "then group by "
        pretty s
      GroupUsing _ s -> do
        write "then group using "
        pretty s
      GroupByUsing _ s t -> do
        write "then group by "
        pretty s
        write " using "
        pretty t

instance Pretty Decl where
  prettyInternal = decl'

-- | Render a declaration.
decl ::  Decl NodeInfo -> Printer ()
decl (InstDecl _ moverlap dhead decls) =
  do depend (write "instance ")
            (depend (maybeOverlap moverlap)
                    (depend (pretty dhead)
                            (unless (null (fromMaybe [] decls))
                                    (write " where"))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
decl (SpliceDecl _ e) = pretty e
decl (TypeSig _ names ty) =
  depend (do inter (write ", ")
                   (map pretty names)
             write " :: ")
         (pretty ty)
decl (FunBind _ matches) =
  lined (map pretty matches)
decl (ClassDecl _ ctx dhead fundeps decls) =
  do classHead ctx dhead fundeps decls
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
decl (TypeDecl _ typehead typ') = do
  write "type "
  pretty typehead
  ifFitsOnOneLineOrElse
    (depend (write " = ") (pretty typ'))
    (do newline
        indentedBlock (depend (write " = ") (pretty typ')))
decl (TypeFamDecl _ declhead result injectivity) = do
  write "type family "
  pretty declhead
  case result of
    Just r -> do
      space
      let sep = case r of
                  KindSig _ _ -> "::"
                  TyVarSig _ _ -> "="
      write sep
      space
      pretty r
    Nothing -> return ()
  case injectivity of
    Just i -> do
      space
      pretty i
    Nothing -> return ()
decl (ClosedTypeFamDecl _ declhead result injectivity instances) = do
  write "type family "
  pretty declhead
  for_ result $ \r -> do
    space
    let sep = case r of
                KindSig _ _ -> "::"
                TyVarSig _ _ -> "="
    write sep
    space
    pretty r
  for_ injectivity $ \i -> do
    space
    pretty i
  space
  write "where"
  newline
  indentedBlock (lined (map pretty instances))
decl (DataDecl _ dataornew ctx dhead condecls mderivs) =
  do depend (do pretty dataornew
                space)
            (withCtx ctx
                     (do pretty dhead
                         case condecls of
                           [] -> return ()
                           [x] -> singleCons x
                           xs -> multiCons xs))
     indentSpaces <- getIndentSpaces
     forM_ mderivs $ \deriv -> newline >> column indentSpaces (pretty deriv)
  where singleCons x =
          do write " ="
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (do newline
                        pretty x)
        multiCons xs =
          do newline
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (depend (write "=")
                            (prefixedLined "|"
                                           (map (depend space . pretty) xs)))

decl (GDataDecl _ dataornew ctx dhead mkind condecls mderivs) =
  do depend (pretty dataornew >> space)
       (withCtx ctx
         (do pretty dhead
             case mkind of
               Nothing -> return ()
               Just kind -> do write " :: "
                               pretty kind
             write " where"))
     indentedBlock $ do
       case condecls of
         [] -> return ()
         _ -> do
           newline
           lined (map pretty condecls)
       forM_ mderivs $ \deriv -> newline >> pretty deriv

decl (InlineSig _ inline active name) = do
  write "{-# "

  unless inline $ write "NO"
  write "INLINE "
  case active of
    Nothing -> return ()
    Just (ActiveFrom _ x) -> write ("[" ++ show x ++ "] ")
    Just (ActiveUntil _ x) -> write ("[~" ++ show x ++ "] ")
  pretty name

  write " #-}"
decl (MinimalPragma _ (Just formula)) =
  wrap "{-# " " #-}" $ do
    depend (write "MINIMAL ") $ pretty formula
decl (ForImp _ callconv maybeSafety maybeName name ty) = do
  string "foreign import "
  pretty' callconv >> space
  case maybeSafety of
    Just safety -> pretty' safety >> space
    Nothing -> return ()
  case maybeName of
    Just namestr -> string (show namestr) >> space
    Nothing -> return ()
  pretty' name
  tyline <- fitsOnOneLine $ do string " :: "
                               pretty' ty
  case tyline of
    Just line -> put line
    Nothing -> do newline
                  indentedBlock $ do string ":: "
                                     pretty' ty
decl (ForExp _ callconv maybeName name ty) = do
  string "foreign export "
  pretty' callconv >> space
  case maybeName of
    Just namestr -> string (show namestr) >> space
    Nothing -> return ()
  pretty' name
  tyline <- fitsOnOneLine $ do string " :: "
                               pretty' ty
  case tyline of
    Just line -> put line
    Nothing -> do newline
                  indentedBlock $ do string ":: "
                                     pretty' ty
decl x' = pretty' x'

classHead
  :: Maybe (Context NodeInfo)
  -> DeclHead NodeInfo
  -> [FunDep NodeInfo]
  -> Maybe [ClassDecl NodeInfo]
  -> Printer ()
classHead ctx dhead fundeps decls = shortHead `ifFitsOnOneLineOrElse` longHead
  where
    shortHead =
      depend
        (write "class ")
        (withCtx ctx $
         depend
           (pretty dhead)
           (depend (unless (null fundeps) (write " | " >> commas (map pretty fundeps)))
              (unless (null (fromMaybe [] decls)) (write " where"))))
    longHead = do
      depend (write "class ") (withCtx ctx $ pretty dhead)
      newline
      indentedBlock $ do
        unless (null fundeps) $ do
          depend (write "| ") (prefixedLined ", " $ map pretty fundeps)
          newline
        unless (null (fromMaybe [] decls)) (write "where")

instance Pretty TypeEqn where
  prettyInternal (TypeEqn _ in_ out_) = do
    pretty in_
    write " = "
    pretty out_

instance Pretty Deriving where
  prettyInternal (Deriving _ strategy heads) =
    depend (write "deriving" >> space) $
    case strategy of
      Nothing -> printHeads
#if MIN_VERSION_haskell_src_exts(1,21,0)
      Just st@(DerivVia _ _) -> printHeads >> space >> pretty st
#endif
      Just st -> depend (pretty st >> space) $ printHeads
    where
      printHeads = do
        let heads' =
              if length heads == 1
                then map stripParens heads
                else heads
        maybeDerives <- fitsOnOneLine $ parens (commas (map pretty heads'))
        case maybeDerives of
          Nothing -> formatMultiLine heads'
          Just derives -> put derives
      stripParens (IParen _ iRule) = stripParens iRule
      stripParens x = x
      formatMultiLine derives = do
        depend (write "( ") $ prefixedLined ", " (map pretty derives)
        newline
        write ")"

instance Pretty DerivStrategy where
  prettyInternal x =
    case x of
      DerivStock _ -> return ()
      DerivAnyclass _ -> write "anyclass"
      DerivNewtype _ -> write "newtype"
#if MIN_VERSION_haskell_src_exts(1,21,0)
      DerivVia _ t -> depend (write "via" >> space) $ pretty t
#endif

instance Pretty Alt where
  prettyInternal x =
    case x of
      Alt _ p galts mbinds ->
        do pretty p
           pretty galts
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                (pretty binds))

#if MIN_VERSION_haskell_src_exts(1,22,0)
instance Pretty Asst where
  prettyInternal x =
    case x of
      ParenA _ asst -> parens $ pretty asst
      IParam _ name ty -> pretty name >> write " :: " >> pretty ty
      TypeA _ ty -> pretty ty
#else
instance Pretty Asst where
  prettyInternal x =
    case x of
      IParam _ name ty -> do
        pretty name
        write " :: "
        pretty ty
      ParenA _ asst -> parens (pretty asst)
#if MIN_VERSION_haskell_src_exts(1,21,0)
      TypeA _ ty -> pretty ty
#else
      ClassA _ name types -> spaced (pretty name : map pretty types)
      i@InfixA {} -> pretty' i
      EqualP _ a b -> do
        pretty a
        write " ~ "
        pretty b
      AppA _ name tys ->
        spaced (pretty name : map pretty tys)
      WildCardA _ name ->
        case name of
          Nothing -> write "_"
          Just n -> do
            write "_"
            pretty n
#endif
#endif

instance Pretty BangType where
  prettyInternal x =
    case x of
      BangedTy _ -> write "!"
      LazyTy _ -> write "~"
      NoStrictAnnot _ -> return ()

instance Pretty Unpackedness where
  prettyInternal (Unpack _) = write "{-# UNPACK #-}"
  prettyInternal (NoUnpack _) = write "{-# NOUNPACK #-}"
  prettyInternal (NoUnpackPragma _) = return ()

instance Pretty Binds where
  prettyInternal x =
    case x of
      BDecls _ ds -> lined (map pretty ds)
      IPBinds _ i -> lined (map pretty i)

instance Pretty ClassDecl where
  prettyInternal x =
    case x of
      ClsDecl _ d -> pretty d
      ClsDataFam _ ctx h mkind ->
        depend
          (write "data ")
          (withCtx
             ctx
             (do pretty h
                 (case mkind of
                    Nothing -> return ()
                    Just kind -> do
                      write " :: "
                      pretty kind)))
      ClsTyFam _ h msig minj ->
        depend
          (write "type ")
          (depend
             (pretty h)
             (depend
                (traverse_
                   (\case
                      KindSig _ kind -> write " :: " >> pretty kind
                      TyVarSig _ tyVarBind -> write " = " >> pretty tyVarBind)
                   msig)
                (traverse_ (\inj -> space >> pretty inj) minj)))
      ClsTyDef _ (TypeEqn _ this that) -> do
        write "type "
        pretty this
        write " = "
        pretty that
      ClsDefSig _ name ty -> do
        write "default "
        pretty name
        write " :: "
        pretty ty

instance Pretty ConDecl where
  prettyInternal x =
    conDecl x

instance Pretty FieldDecl where
  prettyInternal (FieldDecl _ names ty) =
    depend (do commas (map pretty names)
               write " :: ")
           (pretty ty)

instance Pretty FieldUpdate where
  prettyInternal x =
    case x of
      FieldUpdate _ n e ->
        swing (do pretty n
                  write " =")
               (pretty e)
      FieldPun _ n -> pretty n
      FieldWildcard _ -> write ".."

instance Pretty GuardedRhs where
  prettyInternal  =
    guardedRhs

instance Pretty InjectivityInfo where
  prettyInternal x = pretty' x

instance Pretty InstDecl where
  prettyInternal i =
    case i of
      InsDecl _ d -> pretty d
      InsType _ name ty ->
        depend (do write "type "
                   pretty name
                   write " = ")
               (pretty ty)
      _ -> pretty' i

instance Pretty Match where
  prettyInternal = match
    {-case x of
      Match _ name pats rhs' mbinds ->
        do depend (do pretty name
                      space)
                  (spaced (map pretty pats))
           withCaseContext False (pretty rhs')
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                        (pretty binds))
      InfixMatch _ pat1 name pats rhs' mbinds ->
        do depend (do pretty pat1
                      space
                      prettyInfixName name)
                  (do space
                      spaced (map pretty pats))
           withCaseContext False (pretty rhs')
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                        (pretty binds))-}

instance Pretty PatField where
  prettyInternal x =
    case x of
      PFieldPat _ n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun _ n -> pretty n
      PFieldWildcard _ -> write ".."

instance Pretty QualConDecl where
  prettyInternal x =
    case x of
      QualConDecl _ tyvars ctx d ->
        depend (unless (null (fromMaybe [] tyvars))
                       (do write "forall "
                           spaced (map pretty (reverse (fromMaybe [] tyvars)))
                           write ". "))
               (withCtx ctx
                       (pretty d))

instance Pretty GadtDecl where
#if MIN_VERSION_haskell_src_exts(1,21,0)
  prettyInternal (GadtDecl _ name _ _ fields t) =
#else
  prettyInternal (GadtDecl _ name fields t) =
#endif
    horVar `ifFitsOnOneLineOrElse` verVar
    where
      fields' p =
        case fromMaybe [] fields of
          [] -> return ()
          fs -> do
            depend (write "{") $ do
              prefixedLined "," (map (depend space . pretty) fs)
            write "}"
            p
      horVar =
        depend (pretty name >> write " :: ") $ do
          fields' (write " -> ")
          declTy t
      verVar = do
        pretty name
        newline
        indentedBlock $
          depend (write ":: ") $ do
            fields' $ do
              newline
              indented (-3) (write "-> ")
            declTy t

instance Pretty Rhs where
  prettyInternal =
    rhs

instance Pretty Splice where
  prettyInternal x =
    case x of
      IdSplice _ str ->
        do write "$"
           string str
#if MIN_VERSION_haskell_src_exts(1,22,0)
      TIdSplice _ str ->
        do write "$$"
           string str
#endif
      ParenSplice _ e ->
        depend (write "$")
               (parens (pretty e))
#if MIN_VERSION_haskell_src_exts(1,22,0)
      TParenSplice _ e ->
        depend (write "$$")
               (parens (pretty e))
#endif

instance Pretty InstRule where
  prettyInternal (IParen _ rule) = parens $ pretty rule
  prettyInternal (IRule _ mvarbinds mctx ihead) =
    do case mvarbinds of
         Nothing -> return ()
         Just xs -> do write "forall "
                       spaced (map pretty xs)
                       write ". "
       case mctx of
         Nothing -> pretty ihead
         Just ctx -> do
           mst <- fitsOnOneLine (do pretty ctx
                                    write " => "
                                    pretty ihead
                                    write " where")
           case mst of
             Nothing -> withCtx mctx (pretty ihead)
             Just {} -> do
               pretty ctx
               write " => "
               pretty ihead

instance Pretty InstHead where
  prettyInternal x =
    case x of
      -- Base cases
      IHCon _ name -> pretty name
      IHInfix _ typ' name ->
        depend (pretty typ')
               (do space
                   prettyInfixOp name)
      -- Recursive application
      IHApp _ ihead typ' ->
        depend (pretty ihead)
               (do space
                   pretty typ')
      -- Wrapping in parens
      IHParen _ h -> parens (pretty h)

instance Pretty DeclHead where
  prettyInternal x =
    case x of
      DHead _ name -> prettyQuoteName name
      DHParen _ h -> parens (pretty h)
      DHInfix _ var name ->
        do pretty var
           space
           prettyInfixName name
      DHApp _ dhead var ->
        depend (pretty dhead)
               (do space
                   pretty var)

instance Pretty Overlap where
  prettyInternal (Overlap _) = write "{-# OVERLAP #-}"
  prettyInternal (Overlapping _) = write "{-# OVERLAPPING #-}"
  prettyInternal (Overlaps _) = write "{-# OVERLAPS #-}"
  prettyInternal (Overlappable _) = write "{-# OVERLAPPABLE #-}"
  prettyInternal (NoOverlap _) = write "{-# NO_OVERLAP #-}"
  prettyInternal (Incoherent _) = write "{-# INCOHERENT #-}"

instance Pretty Sign where
  prettyInternal (Signless _) = return ()
  prettyInternal (Negative _) = write "-"

instance Pretty CallConv where
  prettyInternal = pretty'

instance Pretty Safety where
  prettyInternal = pretty'

--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers

instance Pretty Module where
  prettyInternal x =
    case x of
      Module _ mayModHead pragmas imps decls ->
        do inter (do newline
                     newline)
                 (mapMaybe (\(isNull,r) ->
                              if isNull
                                 then Nothing
                                 else Just r)
                           [(null pragmas,inter newline (map pretty pragmas))
                           ,(case mayModHead of
                               Nothing -> (True,return ())
                               Just modHead -> (False,pretty modHead))
                           ,(null imps,formatImports imps)
                           ,(null decls
                            ,interOf newline
                                     (map (\case
                                             r@TypeSig{} -> (1,pretty r)
                                             r@InlineSig{} -> (1, pretty r)
                                             r -> (2,pretty r))
                                          decls))])
           newline
        where interOf i ((c,p):ps) =
                case ps of
                  [] -> p
                  _ ->
                    do p
                       replicateM_ c i
                       interOf i ps
              interOf _ [] = return ()
      XmlPage{} -> error "FIXME: No implementation for XmlPage."
      XmlHybrid{} -> error "FIXME: No implementation for XmlHybrid."

-- | Format imports, preserving empty newlines between groups.
formatImports :: [ImportDecl NodeInfo] -> Printer ()
formatImports =
  sequence_ .
  intersperse (newline >> newline) .
  map formatImportGroup . groupAdjacentBy atNextLine
  where
    atNextLine import1 import2 =
      let end1 = srcSpanEndLine (srcInfoSpan (nodeInfoSpan (ann import1)))
          start2 = srcSpanStartLine (srcInfoSpan (nodeInfoSpan (ann import2)))
      in start2 - end1 <= 1
    formatImportGroup imps = do
      shouldSortImports <- gets $ configSortImports . psConfig
      let imps1 =
            if shouldSortImports
              then sortImports imps
              else imps
      sequence_ . intersperse newline $ map formatImport imps1
    moduleVisibleName idecl =
      let ModuleName _ name = importModule idecl
      in name
    formatImport = pretty
    sortImports imps = sortOn moduleVisibleName . map sortImportSpecsOnImport $ imps
    sortImportSpecsOnImport imp = imp { importSpecs = fmap sortImportSpecs (importSpecs imp) }
    sortImportSpecs (ImportSpecList l hiding specs) = ImportSpecList l hiding sortedSpecs
      where
        sortedSpecs = sortBy importSpecCompare . map sortCNames $ specs

        sortCNames (IThingWith l2 name cNames) = IThingWith l2 name . sortBy cNameCompare $ cNames
        sortCNames is = is

groupAdjacentBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjacentBy _ [] = []
groupAdjacentBy adj items = xs : groupAdjacentBy adj rest
  where
    (xs, rest) = spanAdjacentBy adj items

spanAdjacentBy :: (a -> a -> Bool) -> [a] -> ([a], [a])
spanAdjacentBy _ [] = ([], [])
spanAdjacentBy _ [x] = ([x], [])
spanAdjacentBy adj (x:xs@(y:_))
  | adj x y =
    let (xs', rest') = spanAdjacentBy adj xs
    in (x : xs', rest')
  | otherwise = ([x], xs)

importSpecCompare :: ImportSpec l -> ImportSpec l -> Ordering
importSpecCompare (IAbs _ _ (Ident _ s1)) (IAbs _ _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IAbs _ _ (Symbol _ _)) = GT
importSpecCompare (IAbs _ _ (Ident _ s1)) (IThingAll _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IThingAll _ (Symbol _ _)) = GT
importSpecCompare (IAbs _ _ (Ident _ s1)) (IThingWith _ (Ident _ s2) _) = compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IThingWith _ (Symbol _ _) _) = GT
importSpecCompare (IAbs _ _ (Symbol _ _)) (IAbs _ _ (Ident _ _)) = LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IAbs _ _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IAbs _ _ (Symbol _ _)) (IThingAll _ (Ident _ _)) = LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IThingAll _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IAbs _ _ (Symbol _ _)) (IThingWith _ (Ident _ _) _) = LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IThingWith _ (Symbol _ s2) _) = compare s1 s2
importSpecCompare (IAbs _ _ _) (IVar _ _) = LT
importSpecCompare (IThingAll _ (Ident _ s1)) (IAbs _ _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IAbs _ _ (Symbol _ _)) = GT
importSpecCompare (IThingAll _ (Ident _ s1)) (IThingAll _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IThingAll _ (Symbol _ _)) = GT
importSpecCompare (IThingAll _ (Ident _ s1)) (IThingWith _ (Ident _ s2) _) = compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IThingWith _ (Symbol _ _) _) = GT
importSpecCompare (IThingAll _ (Symbol _ _)) (IAbs _ _ (Ident _ _)) = LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IAbs _ _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IThingAll _ (Symbol _ _)) (IThingAll _ (Ident _ _)) = LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IThingAll _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IThingAll _ (Symbol _ _)) (IThingWith _ (Ident _ _) _) = LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IThingWith _ (Symbol _ s2) _) = compare s1 s2
importSpecCompare (IThingAll _ _) (IVar _ _) = LT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IAbs _ _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IAbs _ _ (Symbol _ _)) = GT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IThingAll _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IThingAll _ (Symbol _ _)) = GT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IThingWith _ (Ident _ s2) _) = compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IThingWith _ (Symbol _ _) _) = GT
importSpecCompare (IThingWith _ (Symbol _ _) _) (IAbs _ _ (Ident _ _)) = LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IAbs _ _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IThingWith _ (Symbol _ _) _) (IThingAll _ (Ident _ _)) = LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IThingAll _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IThingWith _ (Symbol _ _) _) (IThingWith _ (Ident _ _) _) = LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IThingWith _ (Symbol _ s2) _) = compare s1 s2
importSpecCompare (IThingWith _ _ _) (IVar _ _) = LT
importSpecCompare (IVar _ (Ident _ s1)) (IVar _ (Ident _ s2)) = compare s1 s2
importSpecCompare (IVar _ (Ident _ _)) (IVar _ (Symbol _ _)) = GT
importSpecCompare (IVar _ (Symbol _ _)) (IVar _ (Ident _ _)) = LT
importSpecCompare (IVar _ (Symbol _ s1)) (IVar _ (Symbol _ s2)) = compare s1 s2
importSpecCompare (IVar _ _) _ = GT

cNameCompare :: CName l -> CName l -> Ordering
cNameCompare (VarName _ (Ident _ s1)) (VarName _ (Ident _ s2)) = compare s1 s2
cNameCompare (VarName _ (Ident _ _)) (VarName _ (Symbol _ _)) = GT
cNameCompare (VarName _ (Ident _ s1)) (ConName _ (Ident _ s2)) = compare s1 s2
cNameCompare (VarName _ (Ident _ _)) (ConName _ (Symbol _ _)) = GT
cNameCompare (VarName _ (Symbol _ _)) (VarName _ (Ident _ _)) = LT
cNameCompare (VarName _ (Symbol _ s1)) (VarName _ (Symbol _ s2)) = compare s1 s2
cNameCompare (VarName _ (Symbol _ _)) (ConName _ (Ident _ _)) = LT
cNameCompare (VarName _ (Symbol _ s1)) (ConName _ (Symbol _ s2)) = compare s1 s2
cNameCompare (ConName _ (Ident _ s1)) (VarName _ (Ident _ s2)) = compare s1 s2
cNameCompare (ConName _ (Ident _ _)) (VarName _ (Symbol _ _)) = GT
cNameCompare (ConName _ (Ident _ s1)) (ConName _ (Ident _ s2)) = compare s1 s2
cNameCompare (ConName _ (Ident _ _)) (ConName _ (Symbol _ _)) = GT
cNameCompare (ConName _ (Symbol _ _)) (VarName _ (Ident _ _)) = LT
cNameCompare (ConName _ (Symbol _ s1)) (VarName _ (Symbol _ s2)) = compare s1 s2
cNameCompare (ConName _ (Symbol _ _)) (ConName _ (Ident _ _)) = LT
cNameCompare (ConName _ (Symbol _ s1)) (ConName _ (Symbol _ s2)) = compare s1 s2

instance Pretty Bracket where
  prettyInternal x =
    case x of
      ExpBracket _ p -> quotation "" (pretty p)
#if MIN_VERSION_haskell_src_exts(1,22,0)
      TExpBracket _ p ->
        brackets
          (depend
             (write "||")
             (do pretty p
                 write "||"))
#endif
      PatBracket _ p -> quotation "p" (pretty p)
      TypeBracket _ ty -> quotation "t" (pretty ty)
      d@(DeclBracket _ _) -> pretty' d

instance Pretty IPBind where
  prettyInternal x =
    case x of
      IPBind _ name expr -> do
        pretty name
        space
        write "="
        space
        pretty expr

instance Pretty BooleanFormula where
  prettyInternal (VarFormula _ i@(Ident _ _)) = pretty' i
  prettyInternal (VarFormula _ (Symbol _ s)) = write "(" >> string s >> write ")"
  prettyInternal (AndFormula _ fs) = do
      maybeFormulas <- fitsOnOneLine $ inter (write ", ") $ map pretty fs
      case maybeFormulas of
        Nothing -> prefixedLined ", " (map pretty fs)
        Just formulas -> put formulas
  prettyInternal (OrFormula _ fs) = do
      maybeFormulas <- fitsOnOneLine $ inter (write " | ") $ map pretty fs
      case maybeFormulas of
        Nothing -> prefixedLined "| " (map pretty fs)
        Just formulas -> put formulas
  prettyInternal (ParenFormula _ f) = parens $ pretty f

--------------------------------------------------------------------------------
-- * Fallback printers

instance Pretty DataOrNew where
  prettyInternal = pretty'

instance Pretty FunDep where
  prettyInternal = pretty'

#if !MIN_VERSION_haskell_src_exts(1,21,0)
instance Pretty Kind where
  prettyInternal = pretty'
#endif

instance Pretty ResultSig where
  prettyInternal (KindSig _ kind) = pretty kind
  prettyInternal (TyVarSig _ tyVarBind) = pretty tyVarBind

instance Pretty Literal where
  prettyInternal (String _ _ rep) = do
    write "\""
    string rep
    write "\""
  prettyInternal (Char _ _ rep) = do
    write "'"
    string rep
    write "'"
  prettyInternal (PrimString _ _ rep) = do
    write "\""
    string rep
    write "\"#"
  prettyInternal (PrimChar _ _ rep) = do
    write "'"
    string rep
    write "'#"
  -- We print the original notation (because HSE doesn't track Hex
  -- vs binary vs decimal notation).
  prettyInternal (Int _l _i originalString) =
    string originalString
  prettyInternal (Frac _l _r originalString) =
    string originalString
  prettyInternal x = pretty' x

instance Pretty Name where
  prettyInternal x = case x of
                          Ident _ _ -> pretty' x -- Identifiers.
                          Symbol _ s -> string s -- Symbols

instance Pretty QName where
  prettyInternal =
    \case
      Qual _ mn n ->
        case n of
          Ident _ i -> do pretty mn; write "."; string i;
          Symbol _ s -> do write "("; pretty mn; write "."; string s; write ")";
      UnQual _ n ->
        case n of
          Ident _ i -> string i
          Symbol _ s -> do write "("; string s; write ")";
      Special _ s@Cons{} -> parens (pretty s)
      Special _ s@FunCon{} -> parens (pretty s)
      Special _ s -> pretty s


instance Pretty SpecialCon where
  prettyInternal s =
    case s of
      UnitCon _ -> write "()"
      ListCon _ -> write "[]"
      FunCon _ -> write "->"
      TupleCon _ Boxed i ->
        string ("(" ++
                replicate (i - 1) ',' ++
                ")")
      TupleCon _ Unboxed i ->
        string ("(# " ++
                replicate (i - 1) ',' ++
                " #)")
      Cons _ -> write ":"
      UnboxedSingleCon _ -> write "(##)"
      ExprHole _ -> write "_"

instance Pretty QOp where
  prettyInternal = pretty'

instance Pretty TyVarBind where
  prettyInternal = pretty'

instance Pretty ModuleHead where
  prettyInternal (ModuleHead _ name mwarnings mexports) =
    do write "module "
       pretty name
       maybe (return ()) pretty mwarnings
       maybe (return ())
             (\exports ->
                do newline
                   indentSpaces <- getIndentSpaces
                   indented indentSpaces (pretty exports))
             mexports
       write " where"

instance Pretty ModulePragma where
  prettyInternal = pretty'

instance Pretty ImportDecl where
  prettyInternal (ImportDecl _ name qualified source safe mpkg mas mspec) = do
    write "import"
    when source $ write " {-# SOURCE #-}"
    when safe $ write " safe"
    when qualified $ write " qualified"
    case mpkg of
      Nothing -> return ()
      Just pkg -> space >> write ("\"" ++ pkg ++ "\"")
    space
    pretty name
    case mas of
      Nothing -> return ()
      Just asName -> do
        space
        write "as "
        pretty asName
    case mspec of
      Nothing -> return ()
      Just spec -> pretty spec

instance Pretty ModuleName where
  prettyInternal (ModuleName _ name) =
    write name

instance Pretty ImportSpecList where
  prettyInternal (ImportSpecList _ hiding spec) = do
    when hiding $ write " hiding"
    let verVar = do
          space
          parens (commas (map pretty spec))
    let horVar = do
          newline
          indentedBlock
            (do depend (write "( ") (prefixedLined ", " (map pretty spec))
                newline
                write ")")
    verVar `ifFitsOnOneLineOrElse` horVar

instance Pretty ImportSpec where
  prettyInternal = pretty'

instance Pretty WarningText where
  prettyInternal (DeprText _ s) =
    write "{-# DEPRECATED " >> string s >> write " #-}"
  prettyInternal (WarnText _ s) =
    write "{-# WARNING " >> string s >> write " #-}"

instance Pretty ExportSpecList where
  prettyInternal (ExportSpecList _ es) = do
    depend (write "(")
           (prefixedLined "," (map pretty es))
    newline
    write ")"

instance Pretty ExportSpec where
  prettyInternal x = string " " >> pretty' x

-- Do statements need to handle infix expression indentation specially because
-- do x *
--    y
-- is two invalid statements, not one valid infix op.
stmt :: Stmt NodeInfo -> Printer ()
stmt (Qualifier _ e@(InfixApp _ a op b)) =
  do col <- fmap (psColumn . snd)
                 (sandbox (write ""))
     infixApp e a op b (Just col)
stmt (Generator _ p e) =
  do indentSpaces <- getIndentSpaces
     pretty p
     indented indentSpaces
              (dependOrNewline
                 (write " <-")
                 space
                 e
                 pretty)
stmt x = case x of
           Generator _ p e ->
             depend (do pretty p
                        write " <- ")
                    (pretty e)
           Qualifier _ e -> pretty e
           LetStmt _ binds ->
             depend (write "let ")
                    (pretty binds)
           RecStmt _ es ->
             depend (write "rec ")
                    (lined (map pretty es))

-- | Make the right hand side dependent if it fits on one line,
-- otherwise send it to the next line.
dependOrNewline
  :: Printer ()
  -> Printer ()
  -> Exp NodeInfo
  -> (Exp NodeInfo -> Printer ())
  -> Printer ()
dependOrNewline left prefix right f =
  do msg <- fitsOnOneLine renderDependent
     case msg of
       Nothing -> do left
                     newline
                     (f right)
       Just st -> put st
  where renderDependent = depend left (do prefix; f right)

-- | Handle do and case specially and also space out guards more.
rhs :: Rhs NodeInfo -> Printer ()
rhs (UnGuardedRhs _ (Do _ dos)) =
  do inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     indentSpaces <- getIndentSpaces
     let indentation | inCase = indentSpaces
                     | otherwise = max 2 indentSpaces
     swingBy indentation
             (write "do")
             (lined (map pretty dos))
rhs (UnGuardedRhs _ e) = do
  msg <-
    fitsOnOneLine
      (do write " "
          rhsSeparator
          write " "
          pretty e)
  case msg of
    Nothing -> swing (write " " >> rhsSeparator) (pretty e)
    Just st -> put st
rhs (GuardedRhss _ gas) =
  do newline
     n <- getIndentSpaces
     indented n
              (lined (map (\p ->
                             do write "|"
                                pretty p)
                          gas))

-- | Implement dangling right-hand-sides.
guardedRhs :: GuardedRhs NodeInfo -> Printer ()
-- | Handle do specially.

guardedRhs (GuardedRhs _ stmts (Do _ dos)) =
  do indented 1
              (do prefixedLined
                    ","
                    (map (\p ->
                            do space
                               pretty p)
                         stmts))
     inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     swing (write "do")
            (lined (map pretty dos))
guardedRhs (GuardedRhs _ stmts e) = do
    mst <- fitsOnOneLine printStmts
    case mst of
      Just st -> do
        put st
        mst' <-
          fitsOnOneLine
            (do write " "
                rhsSeparator
                write " "
                pretty e)
        case mst' of
          Just st' -> put st'
          Nothing -> swingIt
      Nothing -> do
        printStmts
        swingIt
  where
    printStmts =
      indented
        1
        (do prefixedLined
              ","
              (map
                 (\p -> do
                    space
                    pretty p)
                 stmts))
    swingIt = swing (write " " >> rhsSeparator) (pretty e)

match :: Match NodeInfo -> Printer ()
match (Match _ name pats rhs' mbinds) =
  do depend (do case name of
                  Ident _ _ ->
                    pretty name
                  Symbol _ _ ->
                    do write "("
                       pretty name
                       write ")"
                space)
       (spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     for_ mbinds bindingGroup
match (InfixMatch _ pat1 name pats rhs' mbinds) =
  do depend (do pretty pat1
                space
                prettyInfixName name)
            (do space
                spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     for_ mbinds bindingGroup

-- | Format contexts with spaces and commas between class constraints.
context :: Context NodeInfo -> Printer ()
context ctx =
  case ctx of
    CxSingle _ a -> pretty a
    CxTuple _ as -> do
      depend (write "( ") $ prefixedLined ", " (map pretty as)
      newline
      write ")"
    CxEmpty _ -> parens (return ())

typ :: Type NodeInfo -> Printer ()
typ (TyTuple _ Boxed types) = do
  let horVar = parens $ inter (write ", ") (map pretty types)
  let verVar = parens $ prefixedLined "," (map (depend space . pretty) types)
  horVar `ifFitsOnOneLineOrElse` verVar
typ (TyTuple _ Unboxed types) = do
  let horVar = wrap "(# " " #)" $ inter (write ", ") (map pretty types)
  let verVar = wrap "(#" " #)" $ prefixedLined "," (map (depend space . pretty) types)
  horVar `ifFitsOnOneLineOrElse` verVar
typ (TyForall _ mbinds ctx ty) =
  depend (case mbinds of
            Nothing -> return ()
            Just ts ->
              do write "forall "
                 spaced (map pretty ts)
                 write ". ")
         (do indentSpaces <- getIndentSpaces
             withCtx ctx (indented indentSpaces (pretty ty)))
typ (TyFun _ a b) =
  depend (do pretty a
             write " -> ")
         (pretty b)
typ (TyList _ t) = brackets (pretty t)
typ (TyParArray _ t) =
  brackets (do write ":"
               pretty t
               write ":")
typ (TyApp _ f a) = spaced [pretty f, pretty a]
typ (TyVar _ n) = pretty n
typ (TyCon _ p) = pretty p
typ (TyParen _ e) = parens (pretty e)
typ (TyInfix _ a promotedop b) = do
  -- Apply special rules to line-break operators.
  let isLineBreak' op =
        case op of
          PromotedName _ op' -> isLineBreak op'
          UnpromotedName _ op' -> isLineBreak op'
      prettyInfixOp' op =
        case op of
          PromotedName _ op' -> write "'" >> prettyInfixOp op'
          UnpromotedName _ op' -> prettyInfixOp op'
  linebreak <- isLineBreak' promotedop
  if linebreak
    then do pretty a
            newline
            prettyInfixOp' promotedop
            space
            pretty b
    else do pretty a
            space
            prettyInfixOp' promotedop
            space
            pretty b
typ (TyKind _ ty k) =
  parens (do pretty ty
             write " :: "
             pretty k)
typ (TyBang _ bangty unpackty right) =
  do pretty unpackty
     pretty bangty
     pretty right
typ (TyEquals _ left right) =
  do pretty left
     write " ~ "
     pretty right
typ (TyPromoted _ (PromotedList _ _ ts)) =
  do write "'["
     unless (null ts) $ write " "
     commas (map pretty ts)
     write "]"
typ (TyPromoted _ (PromotedTuple _ ts)) =
  do write "'("
     unless (null ts) $ write " "
     commas (map pretty ts)
     write ")"
typ (TyPromoted _ (PromotedCon _ _ tname)) =
  do write "'"
     pretty tname
typ (TyPromoted _ (PromotedString _ _ raw)) = do
  do write "\""
     string raw
     write "\""
typ ty@TyPromoted{} = pretty' ty
typ (TySplice _ splice) = pretty splice
typ (TyWildCard _ name) =
  case name of
    Nothing -> write "_"
    Just n ->
      do write "_"
         pretty n
typ (TyQuasiQuote _ n s) = quotation n (string s)
#if MIN_VERSION_haskell_src_exts(1,20,0)
typ (TyUnboxedSum _ types) = do
  let horVar = wrap "(# " " #)" $ inter (write " | ") (map pretty types)
  let verVar = wrap "(# " " #)" $ prefixedLined "|" (map (depend space . pretty) types)
  horVar `ifFitsOnOneLineOrElse` verVar
#endif
#if MIN_VERSION_haskell_src_exts(1,21,0)
typ (TyStar _) = write "*"
#endif

prettyTopName :: Name NodeInfo -> Printer ()
prettyTopName x@Ident{} = pretty x
prettyTopName x@Symbol{} = parens $ pretty x

-- | Specially format records. Indent where clauses only 2 spaces.
decl' :: Decl NodeInfo -> Printer ()
-- | Pretty print type signatures like
--
-- foo :: (Show x, Read x)
--     => (Foo -> Bar)
--     -> Maybe Int
--     -> (Char -> X -> Y)
--     -> IO ()
--
decl' (TypeSig _ names ty') = do
  mst <- fitsOnOneLine (depend (do commas (map prettyTopName names)
                                   write " :: ")
                               (declTy ty'))
  case mst of
    Nothing -> do
      commas (map prettyTopName names)
      indentSpaces <- getIndentSpaces
      if allNamesLength >= indentSpaces
        then do write " ::"
                newline
                indented indentSpaces (depend (write "   ") (declTy ty'))
        else (depend (write " :: ") (declTy ty'))
    Just st -> put st
  where
    nameLength (Ident _ s) = length s
    nameLength (Symbol _ s) = length s + 2
    allNamesLength = fromIntegral $ sum (map nameLength names) + 2 * (length names - 1)

decl' (PatBind _ pat rhs' mbinds) =
  withCaseContext False $
    do pretty pat
       pretty rhs'
       for_ mbinds bindingGroup

-- | Handle records specially for a prettier display (see guide).
decl' e = decl e

declTy :: Type NodeInfo -> Printer ()
declTy dty =
  case dty of
    TyForall _ mbinds mctx ty ->
      case mbinds of
        Nothing -> do
          case mctx of
            Nothing -> prettyTy False ty
            Just ctx -> do
              mst <- fitsOnOneLine (do pretty ctx
                                       depend (write " => ") (prettyTy False ty))
              case mst of
                Nothing -> do
                  pretty ctx
                  newline
                  indented (-3) (depend (write "=> ") (prettyTy True ty))
                Just st -> put st
        Just ts -> do
          write "forall "
          spaced (map pretty ts)
          write "."
          case mctx of
            Nothing -> do
              mst <- fitsOnOneLine (space >> prettyTy False ty)
              case mst of
                Nothing -> do
                  newline
                  prettyTy True ty
                Just st -> put st
            Just ctx -> do
              mst <- fitsOnOneLine (space >> pretty ctx)
              case mst of
                Nothing -> do
                  newline
                  pretty ctx
                  newline
                  indented (-3) (depend (write "=> ") (prettyTy True ty))
                Just st -> do
                  put st
                  newline
                  indented (-3) (depend (write "=> ") (prettyTy True ty))
    _ -> prettyTy False dty
  where
    collapseFaps (TyFun _ arg result) = arg : collapseFaps result
    collapseFaps e = [e]
    prettyTy breakLine ty = do
      if breakLine
        then
          case collapseFaps ty of
            [] -> pretty ty
            tys -> prefixedLined "-> " (map pretty tys)
        else do
          mst <- fitsOnOneLine (pretty ty)
          case mst of
            Nothing ->
              case collapseFaps ty of
                [] -> pretty ty
                tys -> prefixedLined "-> " (map pretty tys)
            Just st -> put st

-- | Fields are preceded with a space.
conDecl :: ConDecl NodeInfo -> Printer ()
conDecl (RecDecl _ name fields) = do
   pretty name
   newline
   indentedBlock
    (do depend (write "{")
               (prefixedLined ","
                              (map (depend space . pretty) fields))
        newline
        write "}"
        )
conDecl (ConDecl _ name bangty) = do
  prettyQuoteName name
  unless
    (null bangty)
    (ifFitsOnOneLineOrElse
       (do space
           spaced (map pretty bangty))
       (do newline
           indentedBlock (lined (map pretty bangty))))
conDecl (InfixConDecl _ a f b) =
  inter space [pretty a, pretty f, pretty b]

recUpdateExpr :: Printer () -> [FieldUpdate NodeInfo] -> Printer ()
recUpdateExpr expWriter updates = do
  ifFitsOnOneLineOrElse hor $ do
    expWriter
    newline
    indentedBlock (updatesHor `ifFitsOnOneLineOrElse` updatesVer)
  where
    hor = do
      expWriter
      space
      updatesHor
    updatesHor = braces $ commas $ map pretty updates
    updatesVer = do
      depend (write "{ ") $ prefixedLined ", " $ map pretty updates
      newline
      write "}"

--------------------------------------------------------------------------------
-- Predicates

-- | If the given operator is an element of line breaks in configuration.
isLineBreak :: QName NodeInfo -> Printer Bool
isLineBreak (UnQual _ (Symbol _ s)) = do
  breaks <- gets (configLineBreaks . psConfig)
  return $ s `elem` breaks
isLineBreak _ = return False

-- | Does printing the given thing overflow column limit? (e.g. 80)
fitsOnOneLine :: Printer a -> Printer (Maybe PrintState)
fitsOnOneLine p =
  do st <- get
     put st { psFitOnOneLine = True}
     ok <- fmap (const True) p <|> return False
     st' <- get
     put st
     guard $ ok || not (psFitOnOneLine st)
     return (if ok
                then Just st' { psFitOnOneLine = psFitOnOneLine st }
                else Nothing)

-- | If first printer fits, use it, else use the second one.
ifFitsOnOneLineOrElse :: Printer a -> Printer a -> Printer a
ifFitsOnOneLineOrElse a b = do
  stOrig <- get
  put stOrig{psFitOnOneLine = True}
  res <- fmap Just a <|> return Nothing
  case res of
    Just r -> do
      modify $ \st -> st{psFitOnOneLine = psFitOnOneLine stOrig}
      return r
    Nothing -> do
      put stOrig
      guard $ not (psFitOnOneLine stOrig)
      b

bindingGroup :: Binds NodeInfo -> Printer ()
bindingGroup binds =
  do newline
     indented 2
              (do write "where"
                  newline
                  indented 2 (pretty binds))

infixApp :: Exp NodeInfo
         -> Exp NodeInfo
         -> QOp NodeInfo
         -> Exp NodeInfo
         -> Maybe Int64
         -> Printer ()
infixApp e a op b indent =
  hor `ifFitsOnOneLineOrElse` ver
  where
    hor =
      spaced
        [ case link of
          OpChainExp e' -> pretty e'
          OpChainLink qop -> pretty qop
        | link <- flattenOpChain e
        ]
    ver = do
      prettyWithIndent a
      beforeRhs <- case a of
                     Do _ _ -> do
                       indentSpaces <- getIndentSpaces
                       column (fromMaybe 0 indent + indentSpaces + 3) (newline >> pretty op) -- 3 = "do "
                       return space
                     _ -> space >> pretty op >> return newline
      case b of
        Lambda{} -> space >> pretty b
        LCase{} -> space >> pretty b
        Do _ stmts -> swing (write " do") $ lined (map pretty stmts)
        _ -> do
          beforeRhs
          case indent of
            Nothing -> do
              col <- fmap (psColumn . snd)
                          (sandbox (write ""))
              -- force indent for top-level template haskell expressions, #473.
              if col == 0
                then do indentSpaces <- getIndentSpaces
                        column indentSpaces (prettyWithIndent b)
                else prettyWithIndent b
            Just col -> do
              indentSpaces <- getIndentSpaces
              column (col + indentSpaces) (prettyWithIndent b)
    prettyWithIndent e' =
      case e' of
        InfixApp _ a' op' b' -> infixApp e' a' op' b' indent
        _ -> pretty e'

-- | A link in a chain of operator applications.
data OpChainLink l
  = OpChainExp (Exp l)
  | OpChainLink (QOp l)
  deriving (Show)

-- | Flatten a tree of InfixApp expressions into a chain of operator
-- links.
flattenOpChain :: Exp l -> [OpChainLink l]
flattenOpChain (InfixApp _ left op right) =
  flattenOpChain left <>
  [OpChainLink op] <>
  flattenOpChain right
flattenOpChain e = [OpChainExp e]

-- | Write a Template Haskell quotation or a quasi-quotation.
--
-- >>> quotation "t" (string "Foo")
-- > [t|Foo|]
quotation :: String -> Printer () -> Printer ()
quotation quoter body =
  brackets
    (depend
       (do string quoter
           write "|")
       (do body
           write "|"))

-- | Write an UnboxedSum value/pattern.
--
-- >>> unboxedSumValuePattern 0 1 (Var  (UnQual  (Ident  "n")))
-- (# n | #)
-- >>> unboxedSumValuePattern 0 1 (PTuple  Unboxed [PVar  (Ident  "n"),PWildCard ])
-- (# (# n, _ #) | #)
-- >>> unboxedSumValuePattern 1 0 (PVar  (Ident  "b"))
-- (# | b #)
-- >>> unboxedSumValuePattern 1 0 (Var  (UnQual  (Ident  "b")))
-- (# | b #)
unboxedSumValuePattern
  :: (Pretty ast, Show (ast NodeInfo))
  => Int
  -- ^ Number of types from the left.
  -> Int
  -- ^ Number of types from the right.
  -> ast NodeInfo
  -- ^ Value/Pattern.
  -> Printer ()
  -- ^ UnboxedSum Printer.
unboxedSumValuePattern nLeft nRight e = do
  wrap "(# " " #)" $ do
    replicateM_ nLeft (write "| ")
    pretty e
    replicateM_ nRight (write " |")
