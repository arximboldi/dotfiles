--
--  File:       taffybar.hs
--  Author:     Juan Pedro Bolívar Puente <raskolnikov@es.gnu.org>
--  Date:       Sat Jan  3 15:00:03 2015
--
--  Taffybar configuration file.
--

import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.Battery
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.VerticalBar

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk
import Text.Printf

main :: IO ()
main = do
  let bgc = 0.1691
  let bg = (bgc, bgc, bgc)
  let fg = (0.45, 0.45, 0.45, 1)
  let darkText = "#555"
  let lightText = "#999"
  let selected = "#F0544C"

  let memCallback = do
        mi <- parseMeminfo
        return [memoryUsedRatio mi]

      cpuCallback = do
        (_, systemLoad, totalLoad) <- cpuLoad
        return [totalLoad, systemLoad]

      label str color = do
        l <- labelNew (Nothing :: Maybe String)
        labelSetMarkup l (colorize color "" str)
        widgetShowAll l
        return $ toWidget l

  let memCfg = defaultGraphConfig
        { graphDataColors = [ fg ]
        , graphLabel = Just $ colorize darkText "" "  MEM "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }
      cpuCfg = defaultGraphConfig
        { graphDataColors = [ fg
                            , fg
                            ]
        , graphLabel = Just $ colorize darkText "" "  CPU "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }
      logCfg = defaultPagerConfig
        { activeWorkspace = colorize "yellow" "" . escape
        , emptyWorkspace = colorize darkText "" . escape
        , hiddenWorkspace = colorize lightText "" . escape
        , visibleWorkspace = escape
        , widgetSep = colorize darkText "" "   |   "
       }
      batCfg = (defaultBarConfig colorFunc)
        { barBorderColor = bg
        , barBackgroundColor = const bg
        , barPadding = 2
        , barWidth = 1
        }
        where colorFunc pct
               | pct < 0.1 = bg
               | pct < 0.9 = bg
               | otherwise = bg
      mpdCfg = defaultMPRISConfig
        { trackLabel = display
        }
        where artist track  = maybe "[unknown]" id (trackArtist track)
              title  track  = maybe "[unknown]" id (trackTitle  track)
              display track = "<span fgcolor='#D64937'>▶</span>  " ++
                              printf "%s - %s" (artist track) (title track) ++
                              colorize darkText "" "    | "

  let clock = textClockNew Nothing (colorize selected "" "%a %b %_d %H:%M") 1
      logger = taffyPagerNew logCfg
      mpris = mprisNew mpdCfg
      bat = batteryBarNew batCfg 1
      bat' = label "  BAT " darkText
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      sep = label "   |   " darkText
      sep' = label "  " ""
      sep'' = label "   " ""

  defaultTaffybar defaultTaffybarConfig
    { barHeight = 26
    , widgetSpacing = 2
    , startWidgets = [ sep', logger ]
    , endWidgets = [ sep'', clock, sep, tray, sep, bat, bat', mem, cpu
                   , mpris, sep'' ]
    }
