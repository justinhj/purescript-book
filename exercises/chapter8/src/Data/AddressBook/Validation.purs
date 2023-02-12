module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType, address, person, phoneNumber)
import Data.Either (Either)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

-- type Errors = Array String

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneNumbers
           | PhoneField PhoneType

derive instance eqField :: Eq Field

instance showField :: Show Field
  where
    show :: Field -> String
    show FirstNameField = "FirstName"
    show _ = "I dunno"

data ValidationError = ValidationError String Field

type Errors = Array ValidationError

nonEmpty :: Field -> String -> String -> V Errors String
nonEmpty ft field ""     = invalid [ ValidationError ("Field '" <> field <> "' cannot be empty") ft ]
nonEmpty _ _ value  = pure value

validatePhoneNumbers :: String -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field []      =
  invalid [ ValidationError  ("Field '" <> field <> "' must contain at least one value") PhoneNumbers ]
validatePhoneNumbers _     phones  =
  traverse validatePhoneNumber phones

lengthIs :: Field -> String -> Int -> String -> V Errors String
lengthIs ft field len value | length value /= len =
  invalid [ ValidationError ("Field '" <> field <> "' must have length " <> show len) ft ]
lengthIs _ _ _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> String -> Regex -> String -> V Errors String
matches _ _ regex value | test regex value 
                          = pure value
matches ft field _ _ = invalid [ ValidationError ("Field '" <> field <> "' did not match the required format") ft ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty StreetField "Street"  a.street
          <*> nonEmpty CityField "City"    a.city
          <*> lengthIs StateField "State" 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches (PhoneField pn."type") "Number" phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstNameField "First Name" p.firstName
         <*> nonEmpty LastNameField "Last Name" p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers "Phone Numbers" p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
