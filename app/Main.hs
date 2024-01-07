module Main where

-- Define the product of two sets
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- Define the pushout of two sets
{- 
    To generalize pushout, isValidPair function is passed as a parameter. This 
    function take 2 elements from to given sets and return if they are 
    logically valid. isValidPair is defined for each problem and given data.    
-}
pushout :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
pushout isValidPair xs ys = [(x, y) | x <- xs, y <- ys, isValidPair x y]


-- Define all data as Sum type, inspired by execrcise by Prof. Osgood
-- Define the data types, set and valid pair for problem 1
data BirthLocation = BornInCanadaOrToCanadianParents
                   | BornOutsideCanadaAndNotToCanadianParents
                   deriving (Show, Eq)

data ImmigrationStatus = OnVisa
                       | PermanentResident
                       | NativeBornCitizen
                       | NaturalizedCitizenship
                       deriving (Show, Eq)

birthLocations :: [BirthLocation]
birthLocations = [BornInCanadaOrToCanadianParents,
                    BornOutsideCanadaAndNotToCanadianParents]

immigrationStatuses :: [ImmigrationStatus]
immigrationStatuses = [OnVisa, PermanentResident, NativeBornCitizen,
                        NaturalizedCitizenship]


validPairP1 :: BirthLocation -> ImmigrationStatus -> Bool
validPairP1 BornInCanadaOrToCanadianParents NativeBornCitizen = True
validPairP1 BornOutsideCanadaAndNotToCanadianParents is | 
                is == OnVisa || 
                is == PermanentResident || 
                is == NaturalizedCitizenship 
                = True
validPairP1 _ _ = False


-- Define the data types, set and valid pair for problem 2
data AgeGroup = Age0to15
              | Age16to17
              | Age18plus
              deriving (Show, Eq)

data EmploymentStatus = NotEmployable
                      | Unemployed
                      | EmployedPartTime
                      | EmployedFullTime
                      deriving (Show, Eq)

ageGroups :: [AgeGroup]
ageGroups = [Age0to15, Age16to17, Age18plus]

employmentStatuses :: [EmploymentStatus]
employmentStatuses = [NotEmployable, Unemployed, EmployedPartTime, 
                        EmployedFullTime]

validPairP2 :: AgeGroup -> EmploymentStatus -> Bool
validPairP2 Age0to15 NotEmployable = True
validPairP2 Age16to17 is | 
                is == Unemployed ||
                is == EmployedPartTime ||
                is == EmployedFullTime
                = True
validPairP2 Age18plus is |
                is == Unemployed ||
                is == EmployedPartTime ||
                is == EmployedFullTime
                = True
validPairP2 _ _ = False


-- Define the data types, set and valid pair for problem 3
data DiabetesStatus = Normoglycemic
                    | Prediabetes
                    | DiabetesWithoutComplications
                    | DiabetesWithComplications
                    deriving (Show, Eq)

data DiagnosisTreatmentStatus = DiagnosisNotApplicable
                              | UnDiagnosed
                              | DiagnosedNotTreated
                              | DiagnosedTreated
                              deriving (Show, Eq)

diabetesStatuses :: [DiabetesStatus]
diabetesStatuses = [Normoglycemic, Prediabetes, DiabetesWithoutComplications, 
                    DiabetesWithComplications]

diagnosisTreatmentStatuses :: [DiagnosisTreatmentStatus]
diagnosisTreatmentStatuses = [DiagnosisNotApplicable, UnDiagnosed, 
                    DiagnosedNotTreated, DiagnosedTreated]

validPairP3 :: DiabetesStatus -> DiagnosisTreatmentStatus -> Bool
validPairP3 Normoglycemic DiagnosisNotApplicable = True
validPairP3 Prediabetes is |
            is == UnDiagnosed ||
            is == DiagnosedNotTreated ||
            is == DiagnosedTreated
            = True
validPairP3 DiabetesWithoutComplications is |
            is == UnDiagnosed ||
            is == DiagnosedNotTreated ||
            is == DiagnosedTreated
            = True
validPairP3 DiabetesWithComplications is |
            is == UnDiagnosed ||
            is == DiagnosedNotTreated ||
            is == DiagnosedTreated
            = True
validPairP3 _ _ = False


main :: IO ()
main = do
    putStrLn "\n-> Product for problem 1:\n"
    print $ cartesianProduct birthLocations immigrationStatuses
    putStrLn "\n-> Pushout for problem 1:\n"
    print $ pushout validPairP1 birthLocations immigrationStatuses
    putStrLn "\n-> Product for problem 2:\n"
    print $ cartesianProduct ageGroups employmentStatuses
    putStrLn "\n-> Pushout for problem 2:\n"
    print $ pushout validPairP2 ageGroups employmentStatuses
    putStrLn "\n-> Product for problem 3:\n"
    print $ cartesianProduct diabetesStatuses diagnosisTreatmentStatuses
    putStrLn "\n-> Pushout for problem 3:\n"
    print $ pushout validPairP3 diabetesStatuses diagnosisTreatmentStatuses
