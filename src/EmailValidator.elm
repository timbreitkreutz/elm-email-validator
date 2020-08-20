module EmailValidator exposing (..)


allowedLabelChar : Char -> Bool
allowedLabelChar char =
    Char.isAlphaNum char
        || (char == '-')


labelValid : String -> Bool
labelValid label =
    not (String.isEmpty label)
        && not (String.startsWith "-" label)
        && not (String.endsWith "-" label)
        && String.all allowedLabelChar label


domainValid : Maybe String -> Bool
domainValid maybeDomain =
    case maybeDomain of
        Just domain ->
            not (String.isEmpty domain)
                && not (String.startsWith "." domain)
                && not (String.contains "." (String.right 2 domain))
                && String.contains "." domain
                && List.all labelValid (String.split "." domain)

        Nothing ->
            False


allowedLocalChar : Char -> Bool
allowedLocalChar char =
    Char.isAlphaNum char
        || String.contains (String.fromChar char) ".!#$%&'*+/=?^_`{|}~-"


localValid : Maybe String -> Bool
localValid maybeLocal =
    case maybeLocal of
        Just local ->
            not (String.isEmpty local)
                && not (String.startsWith "." local)
                && not (String.endsWith "." local)
                && String.all allowedLocalChar local

        Nothing ->
            False


emailValid : String -> Bool
emailValid email =
    let
        emailParts =
            String.split "@" email

        localPart =
            List.head emailParts

        domainPart =
            case List.tail emailParts of
                Just list ->
                    List.head list

                Nothing ->
                    Nothing
    in
    not (String.isEmpty email)
        && (String.length email < 256)
        && (List.length emailParts == 2)
        && localValid localPart
        && domainValid domainPart
