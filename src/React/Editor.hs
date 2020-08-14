{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module React.Editor where

import React
import React.CoreHooks
import Lib
import React.Vty
import qualified Data.Text.Lazy as TL
import Graphics.Vty as Vty
import Data.Function ((&))
import Data.Bifunctor

data EditorSettings = EditorSettings { editorState :: EditorState, multiline :: Bool }
data EditorState = EditorState { txt :: (TL.Text, TL.Text)}

defaultEditorSettings :: EditorSettings
defaultEditorSettings = EditorSettings (EditorState ("", "")) False

overSelected :: ((TL.Text, TL.Text) -> (TL.Text, TL.Text)) -> EditorState -> EditorState
overSelected f (EditorState {txt, ..}) = (EditorState {txt=f txt, ..})


highlight :: (TL.Text, TL.Text) -> Vty.Image
highlight (before, TL.splitAt 1 -> (focused, after)) =
            txtImg before Vty.<|> Vty.text (Vty.withStyle Vty.defAttr Vty.reverseVideo) (if TL.null focused then focused <> " " else focused) Vty.<|> txtImg after

txtImg = Vty.text Vty.defAttr

mvLeft :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
mvLeft (a, b) = case TL.unsnoc a of
    Nothing -> (a, b)
    Just (a', c) -> (a', TL.cons c b)
mvRight :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
mvRight (a, b) = case TL.uncons b of
    Nothing -> (a, b)
    Just (c, b') -> (TL.snoc a c, b')

-- Need to fix for multiline
beginning :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
beginning (a, b) = ("", a <> b)

-- Need to fix for multiline
end :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
end (a, b) = (a <> b, "")

del :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
del (a, b) = (a, TL.drop 1 b)

delWord :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
delWord ((TL.unsnoc -> Just (a, ' ')), b) = (TL.dropWhileEnd (/= ' ') a, b)
delWord (a, b) = (TL.dropWhileEnd (/= ' ') a, b)

-- Need to fix this for multiline
kill :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
kill (a, b) = (a, TL.dropWhile (/= '\n') b)

-- Need to fix this for multiline
prekill :: (TL.Text, TL.Text) -> (TL.Text, TL.Text)
prekill (a, b) = (TL.dropWhileEnd (/= '\n') a, b)

editor :: Component EditorSettings (TL.Text, Vty.Image)
editor = component $ \(EditorSettings initialEditorState multiline) -> do
    (e, updateEditor) <- useState initialEditorState
    let update = updateEditor . overSelected
    useTermEvent () $ \case
        Vty.EvKey (Vty.KChar 'a') [MCtrl] -> update beginning
        Vty.EvKey (Vty.KChar 'e') [MCtrl] -> update end
        Vty.EvKey (Vty.KChar 'd') [MCtrl] -> update del
        Vty.EvKey (Vty.KChar 'w') [MCtrl] -> update delWord
        Vty.EvKey Vty.KDel _ -> update del
        Vty.EvKey (Vty.KChar 'k') [MCtrl] -> update kill
        Vty.EvKey (Vty.KChar 'u') [MCtrl] -> update prekill
        Vty.EvKey (Vty.KChar c) _ -> update (first (`TL.snoc` c))
        Vty.EvKey Vty.KBS _ -> update (first $ TL.dropEnd 1)
        Vty.EvKey Vty.KEnter _ -> update (first (`TL.snoc` '\n'))
        Vty.EvKey Vty.KLeft _ -> update mvLeft
        Vty.EvKey Vty.KRight _ -> update mvRight
        _ -> return ()

    return $ (uncurry (<>) $ txt e, highlight (txt e))
