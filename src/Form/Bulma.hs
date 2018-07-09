{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Form.Bulma where

import Import

-- rendering
render :: Monad m => FormRender m a
render aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
            $newline never
            \#{fragment}
            $forall view <- views
                <div .field :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                    <label .label for=#{fvId view}>#{fvLabel view}
                    <div .control>
                        ^{fvInput view}
                    $maybe tt <- fvTooltip view
                        <p .help>#{tt}
                    $maybe err <- fvErrors view
                        <p .help .is-danger">#{err}
        |]
    return (res, widget)

-- minimal field setting for specific class attribute
inputSetting :: Text -> FieldSettings site
inputSetting = minimalSetting "input"

textareaSetting :: Text -> FieldSettings site
textareaSetting = minimalSetting "textarea"

selectSetting :: Text -> FieldSettings site
selectSetting = minimalSetting "select"

checkboxSetting :: Text -> FieldSettings site
checkboxSetting = minimalSetting "checkbox"

radioSetting :: Text -> FieldSettings site
radioSetting = minimalSetting "radio"

minimalSetting :: Text -> Text -> FieldSettings site
minimalSetting fClass placeholder = fieldSetting fClass "" Nothing (Just placeholder) Nothing Nothing

-- general FieldSetting
fieldSetting :: Text -> SomeMessage site -> Maybe (SomeMessage site) -> Maybe Text -> Maybe Text -> Maybe Text -> FieldSettings site
fieldSetting fClass msg tooltip placeholder fId name = FieldSettings msg tooltip fId name attrs
    where attrs = ("class", fClass) : maybe [] (\pl -> [("placeholder", pl)]) placeholder