using Documenter
using Revise
using FieldFlags

makedocs(sitename = "FieldFlags.jl",
         format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == "true"),
         pages = [
            "index.md",
            "examples.md",
            "api.md"
])
