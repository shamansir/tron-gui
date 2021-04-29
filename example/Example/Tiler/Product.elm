module Example.Tiler.Product exposing (..)


import Array exposing (..)


type Product
    = JetBrains
    | Space
    | IntelliJ
    | IntelliJEdu
    | PhpStorm
    | PyCharm
    | PyCharmEdu
    | RubyMine
    | WebStorm
    | CLion
    | DataGrip
    | AppCode
    | GoLand
    | ReSharper
    | ReSharperCpp
    | DotCover
    | DotMemory
    | DotPeek
    | DotTrace
    | Rider
    | TeamCity
    | YouTrack
    | Upsource
    | Hub
    | Kotlin
    | MPS
    | Mono
    | Datalore
    | DataSpell
    | Qodana
    | Toolbox
    | Draft


default : Product
default = JetBrains


all : List Product
all =
    [ JetBrains
    , AppCode
    , CLion
    , DataGrip
    , Datalore
    , DataSpell
    , DotCover
    , DotMemory
    , DotPeek
    , DotTrace
    , Draft
    , GoLand
    , Hub
    , IntelliJ
    , IntelliJEdu
    , Kotlin
    , Mono
    , MPS
    , PhpStorm
    , PyCharm
    , PyCharmEdu
    , Qodana
    , ReSharper
    , ReSharperCpp
    , Rider
    , RubyMine
    , Space
    , TeamCity
    , Toolbox
    , Upsource
    , WebStorm
    , YouTrack
    ]


getName : Product -> String
getName product =
    case product of
        JetBrains -> "JetBrains"
        Space -> "Space"
        IntelliJ -> "IntelliJ IDEA"
        PhpStorm -> "PhpStorm"
        PyCharm -> "PyCharm"
        RubyMine -> "RubyMine"
        WebStorm -> "WebStorm"
        CLion -> "CLion"
        DataGrip -> "DataGrip"
        AppCode -> "AppCode"
        GoLand -> "GoLand"
        ReSharper -> "ReSharper"
        ReSharperCpp -> "ReSharper C++"
        DotCover -> "dotCover"
        DotMemory -> "dotMemory"
        DotPeek -> "dotPeek"
        DotTrace -> "dotTrace"
        Rider -> "Rider"
        TeamCity -> "TeamCity"
        Toolbox -> "Toolbox"
        YouTrack -> "YouTrack"
        Upsource -> "Upsource"
        Hub -> "Hub"
        Kotlin -> "Kotlin"
        MPS -> "MPS"
        IntelliJEdu -> "IntelliJ IDEA Edu"
        PyCharmEdu -> "PyCharm Edu"
        Mono -> "Mono"
        Datalore -> "Datalore"
        DataSpell -> "DataSpell"
        Qodana -> "Qodana"
        Draft -> "Draft"


decode : String -> Result String Product
decode id =
    case id of
        "jetbrains" -> Ok JetBrains
        "space" -> Ok Space
        "intellij-idea" -> Ok IntelliJ
        "phpstorm" -> Ok PhpStorm
        "pycharm" -> Ok PyCharm
        "rubymine" -> Ok RubyMine
        "webstorm" -> Ok WebStorm
        "clion" -> Ok CLion
        "datagrip" -> Ok DataGrip
        "appcode" -> Ok AppCode
        "goland" -> Ok GoLand
        "resharper" -> Ok ReSharper
        "resharper-cpp" -> Ok ReSharperCpp
        "dotcover" -> Ok DotCover
        "dotmemory" -> Ok DotMemory
        "dotpeek" -> Ok DotPeek
        "dottrace" -> Ok DotTrace
        "rider" -> Ok Rider
        "teamcity" -> Ok TeamCity
        "toolbox" -> Ok Toolbox
        "youtrack" -> Ok YouTrack
        "upsource" -> Ok Upsource
        "hub" -> Ok Hub
        "kotlin" -> Ok Kotlin
        "mps" -> Ok MPS
        "intellij-idea-edu" -> Ok IntelliJEdu
        "pycharm-edu" -> Ok PyCharmEdu
        "mono" -> Ok Mono
        "datalore" -> Ok Datalore
        "dataspell" -> Ok DataSpell
        "qodana" -> Ok Qodana
        "draft" -> Ok Draft
        _ -> Err id


encode : Product -> String
encode product =
    case product of
        JetBrains -> "jetbrains"
        Space -> "space"
        IntelliJ -> "intellij-idea"
        PhpStorm -> "phpstorm"
        PyCharm -> "pycharm"
        RubyMine -> "rubymine"
        WebStorm -> "webstorm"
        CLion -> "clion"
        DataGrip -> "datagrip"
        AppCode -> "appcode"
        GoLand -> "goland"
        ReSharper -> "resharper"
        ReSharperCpp -> "resharper-cpp"
        DotCover -> "dotcover"
        DotMemory -> "dotmemory"
        DotPeek -> "dotpeek"
        DotTrace -> "dottrace"
        Rider -> "rider"
        TeamCity -> "teamcity"
        Toolbox -> "toolbox"
        YouTrack -> "youtrack"
        Upsource -> "upsource"
        Hub -> "hub"
        Kotlin -> "kotlin"
        MPS -> "mps"
        IntelliJEdu -> "intellij-idea-edu"
        PyCharmEdu -> "pycharm-edu"
        Mono -> "mono"
        Datalore -> "datalore"
        DataSpell -> "dataspell"
        Qodana -> "qodana"
        Draft -> "draft"


twoLetterCode : Product -> String
twoLetterCode product =
    case product of
        JetBrains -> "JETBRAINS_"
        Space -> "SPACE_"
        IntelliJ -> "IJ_"
        PhpStorm -> "PS_"
        PyCharm -> "PC_"
        RubyMine -> "RM_"
        WebStorm -> "WS_"
        CLion -> "CL_"
        DataGrip -> "DG_"
        AppCode -> "AC_"
        GoLand -> "GO_"
        ReSharper -> "R#_"
        ReSharperCpp -> "R++_"
        DotCover -> "DC_"
        DotMemory -> "DM_"
        DotPeek -> "DP_"
        DotTrace -> "DT_"
        Rider -> "RD_"
        TeamCity -> "TC_"
        Toolbox -> "TL_"
        YouTrack -> "YT_"
        Upsource -> "UP_"
        Hub -> "HB_"
        Kotlin -> "KT_"
        MPS -> "MPS_"
        IntelliJEdu -> "IE_"
        PyCharmEdu -> "PE_"
        Mono -> "MN_"
        Datalore -> "DL_"
        DataSpell -> "DS_"
        Qodana -> "QD_"
        Draft -> "DF_"


compare : Product -> Product -> Bool
compare productA productB =
    twoLetterCode productA == twoLetterCode productB
