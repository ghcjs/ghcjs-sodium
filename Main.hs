{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main ( main ) where

import           Control.Applicative
import           Control.Lens               hiding ((<|))
import           Data.Foldable              as Foldable
import           Data.Functor.Contravariant
import           Data.Monoid                hiding (All)
import           Data.Sequence              as Seq
import           Data.Text                  as Text
import           FRP.Sodium

import           Alder.Html
import qualified Alder.Html.Events          as E
import qualified Alder.Html.Elements        as H
import qualified Alder.Html.Attributes      as A
import           Alder.Sodium

-- Data types

data Todo = Todo
    { _todoId    :: Int
    , _title     :: Text
    , _completed :: Bool
    }

data Filter
    = All
    | Active
    | Completed
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data State = State
    { stateTodos      :: Seq Todo
    , stateTodoFilter :: Filter
    , stateEditIndex  :: Maybe Int
    , stateEditText   :: Text
    }

data Command
    = StartEdit Int
    | Edit Text
    | EndEdit Bool
    | Complete Int Bool
    | CompleteAll Bool
    | Destroy Int
    | ChangeFilter Filter
    | ClearCompleted

makeLenses ''Todo
makePrisms ''Command

trim :: Text -> Maybe Text
trim s
    | Text.null t = Nothing
    | otherwise   = Just t
  where
    t = Text.strip s

applyFilter :: Filter -> Todo -> Bool
applyFilter All       = const True
applyFilter Active    = not . view completed
applyFilter Completed = view completed

deleteAt :: Int -> Seq a -> Seq a
deleteAt i s = let (l, r) = Seq.splitAt i s in l >< Seq.drop 1 r

isKey :: Char -> E.KeyboardEvent -> Bool
isKey k e = E.key e == k

-- Views

header :: Input () -> Input Text -> Text -> Html
header onChange onInput inputText =
    H.header !# "header" $ do
        H.h1 $ text "todos"
        H.input !# "new-todo"
                ! A.placeholder "What needs to be done?"
                ! A.autofocus
                ! A.value inputText
                ! A.onInput (E.value >$< onInput)
                ! A.onKeyDown onKeyDown
                ! A.onBlur (() >$ onChange)
  where
    onKeyDown = keep (isKey '\r') (() >$ onChange)

todoItem :: Input Command -> State -> Int -> Todo -> Html
todoItem input s i todo =
    H.li !?. (todo ^. completed, "completed")
         !?. (stateEditIndex s == Just i, "editing")
         ! A.key (todo ^. todoId)
         !? (not $ applyFilter (stateTodoFilter s) todo, A.hidden) $ do
        H.div !. "view" $ do
            H.input !. "toggle"
                    ! A.type_ "checkbox"
                    !? (todo ^. completed, A.checked)
                    ! A.onChange (Complete i . E.checked >$< input)
            H.label ! A.onDoubleClick (StartEdit i >$ input) $
                text $ todo ^. title
            H.button !. "destroy"
                     ! A.onClick (Destroy i >$ input) $ return ()
        H.input !. "edit"
                ! A.value (stateEditText s)
                ! A.onInput (Edit . E.value >$< input)
                ! A.onKeyDown onKeyDown
                ! A.onBlur onBlur
  where
    onBlur    = EndEdit True >$ input
    onKeyDown = keep (isKey '\r')   (EndEdit True  >$ input)
             <> keep (isKey '\ESC') (EndEdit False >$ input)

todoList :: Input Command -> State -> Html
todoList input s =
    H.section !# "main"
              !? (Seq.null $ stateTodos s, A.hidden) $ do
        H.input !# "toggle-all"
                ! A.type_ "checkbox"
                !? (checked, A.checked)
                ! A.onChange (CompleteAll . E.checked >$< input)
        H.label ! A.for "toggle-all" $ text "Mark all as complete"
        H.ul !# "todo-list" $
            iforM_ (stateTodos s) (todoItem input s)
  where
    checked = Foldable.all (view completed) (stateTodos s)

footer :: Input Command -> State -> Html
footer input s =
    H.footer !# "footer"
             !? (Seq.null $ stateTodos s, A.hidden) $ do
        H.span !# "todo-count" $ do
            H.strong $ text (Text.pack $ show active)
            text " item"
            text $ if active == 1 then "" else "s"
            text " left"
        H.ul !# "filters" $ do
            H.li $ filterButton All "All"
            H.li $ filterButton Active "Active"
            H.li $ filterButton Completed "Completed"
        H.button !# "clear-completed"
                 !? (inactive == 0, A.hidden)
                 ! A.onClick (ClearCompleted >$ input) $ do
            text "Clear completed ("
            text (Text.pack $ show inactive)
            text ")"
  where
    filterButton filt t =
        H.button !?. (stateTodoFilter s == filt, "selected")
                 ! A.onClick (ChangeFilter filt >$ input) $ text t

    inactive = Seq.length . Seq.filter (view completed) $ (stateTodos s)
    active   = Seq.length (stateTodos s) - inactive

-- Application

todoApp :: Reactive (Behavior Html)
todoApp = mdo
    (commands, input) <- newInput
    (enter, enterInput) <- newInput
    (change, changeInput) <- newInput

    let submit = filterJust $ trim <$> inputText <@ enter

    nextId <- accum 0 $ (+1) <$ submit

    inputText <- hold "" $
        change <>
        ("" <$ submit)

    todoFilter <- hold All $ select _ChangeFilter commands

    editing <- hold False $
        (True  <$ select _StartEdit commands) <>
        (False <$ select _EndEdit   commands)

    editIndex <- hold 0 $ select _StartEdit commands

    let editIndex' = (\b i -> if b then Just i else Nothing) <$> editing <*> editIndex

    editText <- hold "" $
        ((\l i -> l ^?! ix i . title) <$> todos
            <@> select _StartEdit commands) <>
        select _Edit commands

    todos <- accum Seq.empty $
        ((\i t -> (Todo i t False <|))      <$> nextId <@> submit) <>
        ((\i t -> ix i . title .~ t)        <$> editIndex <*> editText
            <@ filterE id (select _EndEdit commands)) <>
        ((\(i, c) -> ix i . completed .~ c) <$> select _Complete commands) <>
        ((\c -> traverse . completed .~ c)  <$> select _CompleteAll commands) <>
        (deleteAt                           <$> select _Destroy commands) <>
        (Seq.filter (not . view completed)  <$  select _ClearCompleted commands)

    let state = State
            <$> todos
            <*> todoFilter
            <*> editIndex'
            <*> editText

    let render s t = H.section !# "todoapp" $ do
            header enterInput changeInput t
            todoList input s
            footer input s

    return $ render <$> state <*> inputText

main :: IO ()
main = do
    app <- sync todoApp
    mount app
