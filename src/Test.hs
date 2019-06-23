--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ExistentialQuantification #-}

module Test where

import Evaluator

testFunc1 :: LillaVal
testFunc1 = LillaFunc ["v1", "v2"] [
        LillaList [
            AtomicLilla "if",
            LillaList [
                AtomicLilla "primitive",
                AtomicLilla "gt",
                LillaList [
                    AtomicLilla "v1",
                    AtomicLilla "v2"
                ]   
            ],
            LillaList [
                LillaList [
                    AtomicLilla "return",
                    AtomicLilla "v1"
                ]
            ],
            AtomicLilla "else",
            LillaList [
                LillaList [
                    AtomicLilla "return",
                    AtomicLilla "v2"
                ] 
            ]
        ]
    ]

testFunc2 :: LillaVal
testFunc2 = LillaFunc ["v1", "v2"] [
        LillaList [
            AtomicLilla "v3", AtomicLilla "=", LillaList [
                AtomicLilla "primitive", AtomicLilla "plus",
                LillaList [
                    AtomicLilla "v1", AtomicLilla "v2"
                ]
            ]
        ],
        LillaList [
            AtomicLilla "return",
            AtomicLilla "v3"
        ]
    
    ]


testProgram :: LillaProgram
testProgram = [
        LillaList [AtomicLilla "x", AtomicLilla "=", NumericLilla 1],
        LillaList [AtomicLilla "y", AtomicLilla "=", NumericLilla 2],
        LillaList [AtomicLilla "z", AtomicLilla "=", NumericLilla 3],
        LillaList [
            AtomicLilla "if",
            LillaList [
                AtomicLilla "primitive",
                AtomicLilla "gt",
                LillaList [
                    AtomicLilla "y",
                    AtomicLilla "x"
                ]   
                
            ],
            LillaList [
                LillaList [
                    AtomicLilla "x",
                    AtomicLilla "=",
                    LillaList [
                        AtomicLilla "primitive",
                        AtomicLilla "plus",
                        LillaList [
                            AtomicLilla "x",
                            NumericLilla 1
                        ]
                    ]
                ]
            ],
            AtomicLilla "else",
            LillaList [
                LillaList [
                    AtomicLilla "y",
                    AtomicLilla "=",
                    LillaList [
                        AtomicLilla "primitive",
                        AtomicLilla "plus",
                        LillaList [
                            AtomicLilla "y",
                            NumericLilla 1
                        ]
                    ]
                ]
            ]
        ],
        LillaList [AtomicLilla "u", AtomicLilla "=", LillaList [
            AtomicLilla "primitive", 
            AtomicLilla "plus", 
            LillaList [
                AtomicLilla "x",
                AtomicLilla "y"
            ]
        ]],
        LillaList [AtomicLilla "w", AtomicLilla "=", LillaList [
            AtomicLilla "primitive", 
            AtomicLilla "plus", 
            LillaList [
                AtomicLilla "z",
                NumericLilla 1
            ]
        ]],
        LillaList [AtomicLilla "v", AtomicLilla "=", LillaList [
            AtomicLilla "primitive",
            AtomicLilla "gt", 
            LillaList [
                AtomicLilla "x", 
                AtomicLilla "y"
            ]
        ]],
        LillaList [AtomicLilla "k", AtomicLilla "=", LillaList [
                AtomicLilla "add", 
                LillaList [
                    AtomicLilla "u", 
                    AtomicLilla "w"
                ]
            ]
        ],
        LillaList [AtomicLilla "j", AtomicLilla "=", LillaList [
                AtomicLilla "max", 
                LillaList [
                    AtomicLilla "k", 
                    AtomicLilla "u"
                ]
            ]
        ]
    ]

--test :: LillaVal -> LillaEnvironment
--test x = case runStateT (eval x) [] of
--    Left err       -> []
--    Right (_, env) -> env

-- testLillaProgram2 :: LillaProgram
-- testLillaProgram2 = 


testLillaProgram :: LillaProgram -> String
testLillaProgram lp = case execute Global lp (return (Null, env)) of
    Left err       -> show err
    Right (_, env) -> show env
    where 
        env = [("add", testFunc2), ("max", testFunc1)]


