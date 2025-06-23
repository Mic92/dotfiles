function buildBasicParams(from, to) {
  return [
    'sts=true',
    `so=${encodeURIComponent(from.name)}`,
    `zo=${encodeURIComponent(to.name)}`,
    'kl=2', // 2nd class
    'r=13:16:KLASSENLOS:1',
  ];
}

function buildStationParams(from, to) {
  return [
    `soid=A%3D1%40O%3D${encodeURIComponent(from.name)}%40L%3D${from.id}%40`,
    `zoid=A%3D1%40O%3D${encodeURIComponent(to.name)}%40L%3D${to.id}%40`,
    'sot=ST',
    'zot=ST',
    `soei=${from.id}`,
    `zoei=${to.id}`,
  ];
}

function buildTimeParams(departure, arrival) {
  const date = departure || arrival || new Date();
  const [isoDate] = date.toISOString().split('.');
  return [
    `hd=${isoDate}`,
    arrival ? 'hza=A' : 'hza=D',
  ];
}

function buildAdditionalParams() {
  return [
    'hz=%5B%5D', // empty array
    'ar=false', // no arrival
    's=true', // search
    'd=false', // no direct connections only
    'vm=00,01,02,03,04,05,06,07,08,09', // all transport types
    'fm=false', // no first minute
    'bp=false', // no best price
    'dlt=false', // no Deutschland ticket
    'dltv=false', // no Deutschland ticket variant
  ];
}

export default function generateBahnDeUrl(from, to, departure, arrival) {
  const baseUrl = 'https://www.bahn.de/buchung/fahrplan/suche';
  const hashParams = [
    ...buildBasicParams(from, to),
    ...buildStationParams(from, to),
    ...buildTimeParams(departure, arrival),
    ...buildAdditionalParams(),
  ];
  return `${baseUrl}#${hashParams.join('&')}`;
}
