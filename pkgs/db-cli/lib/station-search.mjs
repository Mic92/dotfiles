export async function searchLocation(client, query) {
  try {
    const locations = await client.locations(query, {
      results: 5,
      stops: true,
      addresses: false,
      poi: false,
    });

    // Find the best match (prefer stations/stops)
    const station = locations.find((l) => l.type === 'station' || l.type === 'stop');
    return station || locations[0];
  } catch {
    throw new Error(`Could not find station: ${query}`);
  }
}

export async function handleStationSearch(client, from, to) {
  console.log('Searching for stations...');
  const fromLocation = await searchLocation(client, from);
  const toLocation = await searchLocation(client, to);
  console.log(`From: ${fromLocation.name} (${fromLocation.id})`);
  console.log(`To: ${toLocation.name} (${toLocation.id})`);
  console.log('');
  return {fromLocation, toLocation};
}
