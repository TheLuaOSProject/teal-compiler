local record jitlib
    enum OSName
        "Windows"
        "Linux"
        "OSX"
        "BSD"
        "POSIX"
        "Other"
    end

    on:     function(function | boolean, boolean | nil)
    off:    function(function | boolean, boolean | nil)
    flush:  function(function | boolean, boolean | nil)

    --Status
    status: function(): boolean, string...

    os: OSName
end

global jit: jitlib

return jitlib
