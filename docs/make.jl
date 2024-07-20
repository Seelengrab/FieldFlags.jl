import Pkg

cd(@__DIR__)
Pkg.activate(@__DIR__)
Pkg.develop(path="..")
Pkg.instantiate()

using Documenter
using FieldFlags

DocMeta.setdocmeta!(FieldFlags, :DocTestSetup, :(using FieldFlags); recursive=true)

makedocs(sitename = "FieldFlags.jl",
         format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == "true"),
         repo=Remotes.GitHub("Seelengrab", "FieldFlags.jl"),
         pages = [
            "index.md",
            "examples.md",
            "api.md"
])

!isinteractive() && deploydocs(
   repo = "github.com/Seelengrab/FieldFlags.jl.git",
   devbranch = "main",
   push_preview = true
)
