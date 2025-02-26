module Data.AddressBook.Validation wherevalidatephone

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid)

-----------------
-- Some simple early examples returning `Either` instead of `V`:

-- ANCHOR: nonEmpty1
nonEmpty1 :: String -> Either String String
nonEmpty1 ""     = Left "Field cannot be empty"
nonEmpty1 value  = Right value
-- ANCHOR_END: nonEmpty1

-- ANCHOR: validatePerson1
validatePerson1 :: Person -> Either String Person
validatePerson1 p =
  person <$> nonEmpty1 p.firstName
         <*> nonEmpty1 p.lastName
         <*> pure p.homeAddress
         <*> pure p.phones
-- ANCHOR_END: validatePerson1

-- ANCHOR: validatePerson1Ado
validatePerson1Ado :: Person -> Either String Person
validatePerson1Ado p = ado
  f <- nonEmpty1 p.firstName
  l <- nonEmpty1 p.lastName
  in person f l p.homeAddress p.phones
-- ANCHOR_END: validatePerson1Ado

-----------------

-- ANCHOR: Errors
type Errors
  = Array String
-- ANCHOR_END: Errors

-- ANCHOR: nonEmpty
nonEmpty :: String -> String -> V Errors String
nonEmpty field ""     = invalid [ "Field '" <> field <> "' cannot be empty" ]
nonEmpty _     value  = pure value
-- ANCHOR_END: nonEmpty

-- ANCHOR: validatePhoneNumbers
validatePhoneNumbers :: String -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field []      =
  invalid [ "Field '" <> field <> "' must contain at least one value" ]
validatePhoneNumbers _     phones  =
  traverse validatePhoneNumber phones
-- ANCHOR_END: validatePhoneNumbers

-- ANCHOR: lengthIs
lengthIs :: String -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ "Field '" <> field <> "' must have length " <> show len ]
lengthIs _     _   value = pure value
-- ANCHOR_END: lengthIs

-- ANCHOR: phoneNumberRegex
-- | We use `Data.String.Regex.Unsafe.unsafeRegex` here instead of `Data.String.Regex.regex`
-- | in order to simplify the code.
-- |
-- | The safe function has this signature:
-- |
-- | ```purescript
-- | regex :: String -> RegexFlags -> Either String Regex
-- | ```
-- |
-- | which can fail if passed an invalid regex `String`. This potential failure is worth
-- | checking for at runtime when working with a user-provided regex `String`.
-- | But in our case, we hardcode a literal regex `String`, so it's not as problematic
-- | to use this more convenient "unsafe" version that may throw an exception:
-- |
-- | ```purescript
-- | unsafeRegex :: String -> RegexFlags -> Regex
-- | ```
-- |
-- | We can achieve a bit more safety by binding our `Regex` values at the top level,
-- | so any potential runtime exceptions are thrown as soon as our application starts.
-- | This is better than defining these values in a local context, where the error may
-- | not be encountered until later on during application execution.
phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags
-- ANCHOR_END: phoneNumberRegex

-- ANCHOR: matches
matches :: String -> Regex -> String -> V Errors String
matches _     regex value | test regex value
                          = pure value
matches field _     _     = invalid [ "Field '" <> field <> "' did not match the required format" ]
-- ANCHOR_END: matches

-- ANCHOR: validateAddress
validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty "Street"  a.street
          <*> nonEmpty "City"    a.city
          <*> lengthIs "State" 2 a.state
-- ANCHOR_END: validateAddress

-- ANCHOR: validateAddressAdo
validateAddressAdo :: Address -> V Errors Address
validateAddressAdo a = ado
  street <- nonEmpty "Street"  a.street
  city   <- nonEmpty "City"    a.city
  state  <- lengthIs "State" 2 a.state
  in address street city state
-- ANCHOR_END: validateAddressAdo

-- ANCHOR: validatePhoneNumber
validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches "Number" phoneNumberRegex pn.number
-- ANCHOR_END: validatePhoneNumber

-- ANCHOR: validatePhoneNumberAdo
validatePhoneNumberAdo :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumberAdo pn = ado
  tpe    <- pure pn."type"
  number <- matches "Number" phoneNumberRegex pn.number
  in phoneNumber tpe number
-- ANCHOR_END: validatePhoneNumberAdo

-- ANCHOR: validatePerson
validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty "First Name" p.firstName
         <*> nonEmpty "Last Name" p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers "Phone Numbers" p.phones
-- ANCHOR_END: validatePerson

-- ANCHOR: validatePersonAdo
validatePersonAdo :: Person -> V Errors Person
validatePersonAdo p = ado
  firstName <- nonEmpty "First Name" p.firstName
  lastName  <- nonEmpty "Last Name" p.lastName
  address   <- validateAddress p.homeAddress
  numbers   <- validatePhoneNumbers "Phone Numbers" p.phones
  in person firstName lastName address numbers
-- ANCHOR_END: validatePersonAdo
