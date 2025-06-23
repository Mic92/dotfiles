import {formatDuration} from './time-parser.mjs';

const LINE_WIDTH = 80;

function displayJourneyLeg(leg) {
  const depTime = new Date(leg.departure).toLocaleTimeString('de-DE', {hour: '2-digit', minute: '2-digit'});
  const arrTime = new Date(leg.arrival).toLocaleTimeString('de-DE', {hour: '2-digit', minute: '2-digit'});
  const platform = leg.departurePlatform ? ` (Platform ${leg.departurePlatform})` : '';

  if (leg.walking) {
    console.log(`  ${depTime} - ${arrTime}: Walk from ${leg.origin.name} to ${leg.destination.name}`);
  } else if (leg.line) {
    const lineName = leg.line.name || `${leg.line.productName} ${leg.line.fahrtNr}`;
    console.log(`  ${depTime} - ${arrTime}: ${lineName} from ${leg.origin.name}${platform} to ${leg.destination.name}`);

    // Show delays if any
    if (leg.departureDelay) {
      console.log(`    ⚠️  Departure delay: +${Math.round(leg.departureDelay / 60)} min`);
    }
    if (leg.arrivalDelay) {
      console.log(`    ⚠️  Arrival delay: +${Math.round(leg.arrivalDelay / 60)} min`);
    }
  }
}

export function displayJourney(journey, index) {
  console.log(`\nJourney ${index + 1}:`);

  // Calculate total duration
  const startTime = new Date(journey.legs[0].departure);
  const endTime = new Date(journey.legs[journey.legs.length - 1].arrival);
  const duration = Math.round((endTime - startTime) / (1000 * 60));

  const timeOptions = {hour: '2-digit', minute: '2-digit'};
  console.log(`${startTime.toLocaleTimeString('de-DE', timeOptions)} → ${endTime.toLocaleTimeString('de-DE', timeOptions)} (${formatDuration(duration)})`);
  console.log(`Changes: ${journey.legs.length - 1}`);

  // Show journey legs
  journey.legs.forEach(displayJourneyLeg);

  // Show warnings/remarks
  if (journey.remarks && journey.remarks.length > 0) {
    journey.remarks.forEach((remark) => {
      if (remark.type === 'warning') {
        console.log(`  ⚠️  ${remark.text}`);
      }
    });
  }

  console.log('-'.repeat(LINE_WIDTH));
}

export function displayResults(journeys, locations, bahnUrl) {
  console.log(`Found ${journeys.length} journey${journeys.length > 1 ? 's' : ''}\n`);
  console.log('='.repeat(LINE_WIDTH));
  journeys.forEach(displayJourney);
  console.log(`\n${'='.repeat(LINE_WIDTH)}`);
  console.log('\nView on bahn.de:');
  console.log(bahnUrl);
}
