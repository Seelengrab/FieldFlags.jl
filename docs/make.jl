using Documenter
using FieldFlags

DocMeta.setdocmeta!(FieldFlags, :DocTestSetup, :(using FieldFlags); recursive=true)

makedocs(modules=[FieldFlags],
         sitename = "FieldFlags.jl",
         format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == "true"),
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
