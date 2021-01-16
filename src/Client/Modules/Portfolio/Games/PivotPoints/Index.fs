module PivotPoints

open System

// TODO

// This game uses the same grid list idea for positions as GoalRoll and TileSort
    // rolls in set direction until pivoted in new direction
        // Once a roll is initiated in a direction, will only roll forward in that direction
        // must pivot out of way of obstacles: blockers
        // collect coins: list of int positions that spawn coins in order. Must collect each coin before the next spawns.
            // speeds up like snake as more are picked up
                // certain coins have certain effects (?)
                    // turns to blocker
                    // speed up round
                    // reverse roll direction
                    // etc..(?)
        // pivot can be to either side of the roll direction: 
            // rolling up / down in column -> can roll left or right to row
            // rolling left / right in row -> can roll up or down to column
        