#!/usr/bin/env node

import {parseArgs} from 'node:util';
import process from 'node:process';
import {searchJourneys} from '../lib/index.mjs';

const HELP_TEXT = `
Journey CLI - Search for train connections in Germany

Usage: db-cli [options] <from> <to>

Arguments:
  from                     Origin station name or ID
  to                       Destination station name or ID

Options:
  -d, --departure <time>   Departure time (ISO format or relative like "in 2 hours")
  -a, --arrival <time>     Arrival time (ISO format or relative like "by 18:00")
  -r, --results <number>   Number of results to show (default: 3)
  -h, --help              Show this help message

Examples:
  db-cli "Berlin Hbf" "München Hbf"
  db-cli --departure "2024-12-25T14:00" Berlin München
  db-cli -d "in 30 minutes" "Hamburg Hbf" "Frankfurt Hbf"
  db-cli --arrival "18:00" Köln Stuttgart
`;

function parseCommandLineArgs() {
  const options = {
    departure: {
      type: 'string',
      short: 'd',
    },
    arrival: {
      type: 'string',
      short: 'a',
    },
    results: {
      type: 'string',
      short: 'r',
      default: '3',
    },
    help: {
      type: 'boolean',
      short: 'h',
    },
  };

  return parseArgs({
    options,
    allowPositionals: true,
  });
}

async function main() {
  try {
    const {values, positionals} = parseCommandLineArgs();
    
    if (values.help || positionals.length < 2) {
      console.log(HELP_TEXT);
      return;
    }
    
    if (values.departure && values.arrival) {
      throw new Error('Cannot specify both departure and arrival time');
    }

    const [from, to] = positionals;
    
    await searchJourneys({
      from,
      to,
      departure: values.departure,
      arrival: values.arrival,
      results: parseInt(values.results, 10),
    });
  } catch (error) {
    console.error(`Error: ${error.message}`);
    if (error.isHafasError) {
      console.error(`API Error: ${error.hafasMessage}`);
    }
    process.exit(1);
  }
}

main();