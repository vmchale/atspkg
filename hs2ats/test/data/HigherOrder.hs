data GenericSum a b = GenericL a
                    | GenericR b

newtype GenericSumInt = GenericSumInt (GenericSum String Int)
