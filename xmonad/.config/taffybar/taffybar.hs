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
  let bgc = 0.11372
  let bg = (bgc, bgc, bgc)
  let fg = (0.4, 0.4, 0.4, 1)

  let memCallback = do
        mi <- parseMeminfo
        return [memoryUsedRatio mi]

      cpuCallback = do
        (_, systemLoad, totalLoad) <- cpuLoad
        return [totalLoad, systemLoad]

      label str color = do
        l <- labelNew Nothing
        labelSetMarkup l (colorize color "" str)
        widgetShowAll l
        return $ toWidget l

  let memCfg = defaultGraphConfig
        { graphDataColors = [ fg ]
        , graphLabel = Just $ colorize "#444" "" "  MEM "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }
      cpuCfg = defaultGraphConfig
        { graphDataColors = [ fg
                            , fg
                            ]
        , graphLabel = Just $ colorize "#444" "" "  CPU "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }
      logCfg = defaultPagerConfig
        { activeWorkspace = colorize "yellow" "" . escape
        , emptyWorkspace = colorize "#444" "" . escape
        , hiddenWorkspace = colorize "#888" "" . escape
        , visibleWorkspace = escape
        , widgetSep = colorize "#444" "" "   |   "
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
                              colorize "#444" "" "    | "

  let clock = textClockNew Nothing (colorize "#D64937" "" "%a %b %_d %H:%M") 1
      logger = taffyPagerNew logCfg
      mpris = mprisNew mpdCfg
      bat = batteryBarNew batCfg 1
      bat' = label "  BAT " "#444"
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      sep = label "   |   " "#444"
      sep' = label "  " ""
      sep'' = label "   " ""

  defaultTaffybar defaultTaffybarConfig
    { barHeight = 22
    , widgetSpacing = 2
    , startWidgets = [ sep', logger ]
    , endWidgets = [ sep'', clock, sep, tray, sep, bat, bat', mem, cpu
                   , mpris, sep'' ]
    }
