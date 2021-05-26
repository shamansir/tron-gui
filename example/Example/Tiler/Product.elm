module Example.Tiler.Product exposing (..)


import Color exposing (Color)
import Color.Convert exposing (hexToColor)
import Array exposing (..)


type Palette = Palette Color Color Color


type ColorId
    = ColorI
    | ColorII
    | ColorIII


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
    | Datalore


default : Product
default = JetBrains


all : List Product
all =
    [ JetBrains
    , Space
    , IntelliJ
    , PhpStorm
    , PyCharm
    , RubyMine
    , WebStorm
    , CLion
    , DataGrip
    , AppCode
    , GoLand
    , ReSharper
    , ReSharperCpp
    , DotCover
    , DotMemory
    , DotPeek
    , DotTrace
    , Rider
    , TeamCity
    , YouTrack
    , Upsource
    , Hub
    , Kotlin
    , MPS
    , IntelliJEdu
    , PyCharmEdu
    , Datalore


    {-
    , AppCode
    , CLion
    , DataGrip
    , Datalore
    , DotCover
    , DotMemory
    , DotPeek
    , DotTrace
    , GoLand
    , Hub
    , IntelliJ
    , IntelliJEdu
    , Kotlin
    , MPS
    , PhpStorm
    , PyCharm
    , PyCharmEdu
    , ReSharper
    , ReSharperCpp
    , Rider
    , RubyMine
    , Space
    , TeamCity
    , Upsource
    , WebStorm
    , YouTrack -}
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
        YouTrack -> "YouTrack"
        Upsource -> "Upsource"
        Hub -> "Hub"
        Kotlin -> "Kotlin"
        MPS -> "MPS"
        IntelliJEdu -> "IntelliJ IDEA Edu"
        PyCharmEdu -> "PyCharm Edu"
        Datalore -> "Datalore"


iconName : Product -> Maybe String
iconName product =
    case product of
        JetBrains -> Just "logojb"
        Space -> Just "Space"
        IntelliJ -> Just "IntelliJ-IDEA"
        PhpStorm -> Just "PhpStorm"
        PyCharm -> Just "PyCharm"
        RubyMine -> Just "RubyMine"
        WebStorm -> Just "WebStorm"
        CLion -> Just "CLion"
        DataGrip -> Just "DataGrip"
        AppCode -> Just "AppCode"
        GoLand -> Just "GoLand"
        ReSharper -> Just "ReSharper"
        ReSharperCpp -> Just "ReSharperCPP"
        DotCover -> Just "dotCover"
        DotMemory -> Just "dotMemory"
        DotPeek -> Just "dotPeek"
        DotTrace -> Just "dotTrace"
        Rider -> Just "Rider"
        TeamCity -> Just "TeamCity"
        YouTrack -> Just "YouTrack"
        Upsource -> Just "Upsource"
        Hub -> Just "Hub"
        Kotlin -> Just "Kotlin"
        MPS -> Just "MPS"
        IntelliJEdu -> Just "IntelliJ-IDEA-Edu"
        PyCharmEdu -> Just "PyCharm-Edu"
        Datalore -> Just "Datalore"


hasIcon : Product -> Bool
hasIcon product =
    case iconName product of
        Just _ -> True
        Nothing -> False


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
        "youtrack" -> Ok YouTrack
        "upsource" -> Ok Upsource
        "hub" -> Ok Hub
        "kotlin" -> Ok Kotlin
        "mps" -> Ok MPS
        "intellij-idea-edu" -> Ok IntelliJEdu
        "pycharm-edu" -> Ok PyCharmEdu
        "datalore" -> Ok Datalore
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
        YouTrack -> "youtrack"
        Upsource -> "upsource"
        Hub -> "hub"
        Kotlin -> "kotlin"
        MPS -> "mps"
        IntelliJEdu -> "intellij-idea-edu"
        PyCharmEdu -> "pycharm-edu"
        Datalore -> "datalore"


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
        YouTrack -> "YT_"
        Upsource -> "UP_"
        Hub -> "HB_"
        Kotlin -> "KT_"
        MPS -> "MPS_"
        IntelliJEdu -> "IE_"
        PyCharmEdu -> "PE_"
        Datalore -> "DL_"


compare : Product -> Product -> Bool
compare productA productB =
    twoLetterCode productA == twoLetterCode productB


getPalette : Product -> Palette
getPalette product =
    let
        p s1 s2 s3 =
            Result.map3
                Palette
                (hexToColor s1)
                (hexToColor s2)
                (hexToColor s3)
            |> Result.withDefault (Palette Color.white Color.white Color.white)
    in case product of
        JetBrains -> p "#ed3d7d" "#7c59a4" "#fcee39"
        Space ->     p "#003CB7" "#5FCCF5" "#ADF03E"
        IntelliJ ->  p "#007efc" "#fe315d" "#f97a12"
        PhpStorm ->  p "#b345f1" "#765af8" "#ff318c"
        PyCharm ->   p "#21d789" "#fcf84a" "#07c3f2"
        RubyMine ->  p "#fe2857" "#fc801d" "#9039d0"
        WebStorm ->  p "#07c3f2" "#087cfa" "#fcf84a"
        CLion ->     p "#21d789" "#009ae5" "#ed358c"
        DataGrip ->  p "#22d88f" "#9775f8" "#ff318c"
        AppCode ->   p "#087cfa" "#07c3f2" "#21d789"
        GoLand ->    p "#0d7bf7" "#b74af7" "#3bea62"
        ReSharper -> p "#c21456" "#e14ce3" "#fdbc2c"
        ReSharperCpp -> p "#fdbc2c" "#e14ce3" "#c21456"
        DotCover ->  p "#ff7500" "#7866ff" "#e343e6"
        DotMemory -> p "#ffbd00" "#7866ff" "#e343e6"
        DotPeek ->   p "#00caff" "#7866ff" "#e343e6"
        DotTrace ->  p "#fc1681" "#786bfb" "#e14ce3"
        Rider ->     p "#c90f5e" "#077cfb" "#fdb60d"
        TeamCity ->  p "#0cb0f2" "#905cfb" "#3bea62"
        YouTrack ->  p "#0cb0f2" "#905cfb" "#ff318c"
        Upsource ->  p "#22b1ef" "#9062f7" "#fd8224"
        Hub ->       p "#00b8f1" "#9758fb" "#ffee45"
        Kotlin ->    p "#627cf7" "#d44ea3" "#ff6925"
        MPS ->       p "#0b8fff" "#21d789" "#ffdc52"
        IntelliJEdu ->  p "#0d7bf7" "#fe315d" "#f97a12"
        PyCharmEdu ->   p "#21d789" "#fcf84a" "#07c3f2"
        Datalore ->   p "#3bea62" "#6b57ff" "#07c3f2"


getPaletteColor : ColorId -> Palette -> Color
getPaletteColor colorId (Palette color1 color2 color3) =
    case colorId of
        ColorI   -> color1
        ColorII  -> color2
        ColorIII -> color3



paletteToList : Palette -> List Color
paletteToList (Palette c1 c2 c3) =
    [ c1, c2, c3 ]
