module Task1 where

data Lord = Exists | LordIsAbsent
data Castle = Castle Lord | CastleIsAbsent
data PublicBuilding = Library | Church | PbIsAbsent

data Resident = Resident
data Residents = One                             Resident |
                 Two                    Resident Resident |
                 Three         Resident Resident Resident |
                 Four Resident Resident Resident Resident

data LivingHouse = LivingHouse Residents
data LivingHouses = OneHouse LivingHouse | FewHouses LivingHouse LivingHouses

data City = City          Castle PublicBuilding LivingHouses |
            CityWithWalls Castle PublicBuilding LivingHouses

data ActionResult a = OK a | ANothing a String

buildCastle :: City -> ActionResult City
buildCastle (City castle pb lh) =
  case castle of
    Castle _       -> ANothing (City castle pb lh) "Castle already exists"
    CastleIsAbsent -> OK (City (Castle LordIsAbsent) pb lh)
buildCastle (CityWithWalls castle pb lh) =
  case castle of
    Castle _       -> ANothing (CityWithWalls castle pb lh) "Castle already exists"
    CastleIsAbsent -> OK (CityWithWalls (Castle LordIsAbsent) pb lh)

buildChurch :: City -> ActionResult City
buildChurch = buildPublicBuilding Church

buildLibrary :: City -> ActionResult City
buildLibrary = buildPublicBuilding Library

buildPublicBuilding :: PublicBuilding -> City -> ActionResult City
buildPublicBuilding newPb (City c pb lh) =
  case pb of
    PbIsAbsent -> OK (City c newPb lh)
    _          -> ANothing (City c pb lh) "Public building already exists"
buildPublicBuilding newPb (CityWithWalls c pb lh) =
  case pb of
    PbIsAbsent -> OK (CityWithWalls c newPb lh)
    _          -> ANothing (CityWithWalls c pb lh) "Public building already exists"

buildLivingHouse :: Residents -> City -> City
buildLivingHouse residents (City c pb livingHouses) =
  let newLivingHouses = FewHouses (LivingHouse residents) livingHouses in
  (City c pb newLivingHouses)
buildLivingHouse residents (CityWithWalls c pb livingHouses) =
  let newLivingHouses = FewHouses (LivingHouse residents) livingHouses in
  (CityWithWalls c pb newLivingHouses)

putTheLordInTheCastle :: City -> ActionResult City
putTheLordInTheCastle (City castle pb lh) =
  case castle of
    Castle LordIsAbsent -> OK (City (Castle Exists) pb lh)
    Castle Exists       -> ANothing (City castle pb lh) "Lord alredy exists"
    CastleIsAbsent      -> ANothing (City castle pb lh) "There is not castle"
putTheLordInTheCastle (CityWithWalls castle pb lh) =
  case castle of
    Castle LordIsAbsent -> OK (CityWithWalls (Castle Exists) pb lh)
    Castle Exists       -> ANothing (CityWithWalls castle pb lh) "Lord alredy exists"
    CastleIsAbsent      -> ANothing (CityWithWalls castle pb lh) "There is not castle"

buildWalls :: City -> ActionResult City
buildWalls (CityWithWalls c pb lh) = ANothing (CityWithWalls c pb lh) "Walls is already exists"
buildWalls (City castle pb lh)     =
  if (lordExists castle) then
    if (atLeastTenPeople lh) then
      OK (CityWithWalls castle pb lh)
    else
      ANothing (City castle pb lh) "There's less then 10 people"
  else
    ANothing (City castle pb lh) "There are not lord"

lordExists :: Castle -> Bool
lordExists (Castle lord) =
  case lord of
    Exists -> True
    _      -> False
lordExists CastleIsAbsent = False

atLeastTenPeople :: LivingHouses -> Bool
atLeastTenPeople lh =
  if ((countPeople lh) >= 10)then True
  else False

countPeople :: LivingHouses -> Int
countPeople (OneHouse house) = countPeopleInTheHouse house
countPeople (FewHouses house otherHouses) = (countPeopleInTheHouse house) + (countPeople otherHouses)

countPeopleInTheHouse :: LivingHouse -> Int
countPeopleInTheHouse (LivingHouse residents) =
  case residents of
    (One   _      ) -> 1
    (Two   _ _    ) -> 2
    (Three _ _ _  ) -> 3
    (Four  _ _ _ _) -> 4
