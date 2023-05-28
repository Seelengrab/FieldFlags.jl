using Documenter
using FieldFlags

DocMeta.setdocmeta!(FieldFlags, :DocTestSetup, :(using FieldFlags); recursive=true)

makedocs(sitename = "FieldFlags.jl",
         format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == "true"),
         pages = [
            "index.md",
            "examples.md",
            "api.md"
])
