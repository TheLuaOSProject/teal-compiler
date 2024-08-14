return {
    build_dir = "build",
    source_dir = "src",
    include_dir = {
        "teal/",
        "types/",
        "src/"
    },
    scripts = {
        build = {
            pre = "scripts/force-good-tl.lua",
        }
    },
    gen_target = "5.1"
}
