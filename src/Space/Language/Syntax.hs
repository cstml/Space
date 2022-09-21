module Space.Language.Syntax where

import Space.Language.Variable

data TermSyntax a =
  Line (TermSyntax a)
  | Sequence a (TermSyntax a)
  | ReverseSequence a a (TermSyntax a)
  | Module ModuleName ExportedVariables (TermSyntax a)

newtype ModuleName = ModuleName {getModuleName :: String}
newtype ExportedVariables = ExportedVariables {getExportedVariables :: [Variable]}
