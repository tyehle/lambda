module HL.Type where


data PolyType = Forall [String] KType deriving (Eq, Show)

data KType = TApp Kind KType KType
           | TLeaf Kind String
           deriving (Eq, Show)

data Kind = Concrete | KApp Kind Kind | KUnknown deriving (Eq, Show)

infixl 1 ~>
(~>) :: Kind -> Kind -> Kind
a ~> b = KApp a b

o :: Kind
o = Concrete

k :: Kind
k = KUnknown

inferKinds :: [PolyType] -> PolyType -> PolyType
inferKinds _ _ = undefined



monoType :: Kind -> String -> PolyType
monoType kind name = Forall [] $ TLeaf kind name

bool :: PolyType
bool = monoType o "Bool"

list :: PolyType
list = monoType (o ~> o) "List"

num :: PolyType
num = monoType o "Num"

-- unit :: PolyType
-- unit = monoType o "Unit"

function :: PolyType
function = monoType (o ~> o ~> o) "->"
