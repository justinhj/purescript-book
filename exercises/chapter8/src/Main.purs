module Main where

import Prelude

import Data.AddressBook (PhoneNumber, examplePerson)
import Data.AddressBook.Validation (Errors, Field(..), ValidationError(..), validatePerson')
import Data.Array (find, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- matchField :: ValidationError -> Field -> Boolean
-- matchField (ValidationError _ ft) ft' | ft == ft' = true
-- matchField _ _ = false

matchError :: Field -> Errors -> Maybe ValidationError
matchError f errs = find (\(ValidationError _ x) -> x == f) errs

renderError :: Field -> Errors -> R.JSX
renderError _ [] = D.div { }
renderError field errors  =
        case matchError field errors of
          Just (ValidationError str _) -> 
            D.div {
              className: "alert alert-danger row", children: [ D.text str ]
              }
          Nothing -> D.div {
              className: "", children: [ ]
          }

-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
-- ANCHOR: renderValidationErrors
-- renderValidationErrors :: Errors -> Array R.JSX
-- renderValidationErrors [] = []
-- renderValidationErrors xs =
--   let
--     renderError :: ValidationError -> R.JSX
--     renderError (ValidationError err ft) = D.div {
--         className: "alert alert-danger row"
--         , children: [ D.text (err <> (show ft)) ]
--         }
--   in
--     [ D.div_ (map renderError xs) ]
-- ANCHOR_END: renderValidationErrors

-- Helper function to render a single form field with an
-- event handler to update
-- Also a bunch of errors lol
formField :: Field -> Errors -> String -> String -> String -> (String -> Effect Unit) -> R.JSX
formField ft errors name placeholder value setValue =
  D.label
    { className: "form-group row"
    , children:
        [ D.div
            { className: "col-sm col-form-label"
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , placeholder
                    , value
                    , onChange:
                        let
                          handleValue :: Maybe String -> Effect Unit
                          handleValue (Just v) = setValue v
                          handleValue Nothing  = pure unit
                        in
                          handler targetValue handleValue
                    }
                ]
            }
          , renderError ft errors
        ]
    }

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  reactComponent "AddressBookApp" \_ -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left  e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        formField (PhoneField phone."type") errors
          (show phone."type")
          "XXX-XXX-XXXX"
          phone.number
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber person.phones
    -- ANCHOR: mkAddressBookApp_pure
    pure
      $ D.div
          { className: "container"
          , children:
                [ D.div
                      { className: "row"
                      , children:
                          [ D.form_
                              $ [ D.h3_ [ D.text "Basic Information" ]
                                , formField FirstNameField errors "First Name" "First Name" person.firstName \s ->
                                    setPerson _ { firstName = s }
                                , formField LastNameField errors "Last Name" "Last Name" person.lastName \s ->
                                    setPerson _ { lastName = s }
                                , D.h3_ [ D.text "Address" ]
                                , formField StreetField errors "Street" "Street" person.homeAddress.street \s ->
                                    setPerson _ { homeAddress { street = s } }
                                , formField CityField errors "City" "City" person.homeAddress.city \s ->
                                    setPerson _ { homeAddress { city = s } }
                                , formField StateField errors "State" "State" person.homeAddress.state \s ->
                                    setPerson _ { homeAddress { state = s } }
                                , D.h3_ [ D.text "Contact Information" ]
                                ]
                              <> renderPhoneNumbers
                          ]
                      }
                  ]
          }
    -- ANCHOR_END: mkAddressBookApp_pure

-- ANCHOR: main
main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
-- ANCHOR_END: main
