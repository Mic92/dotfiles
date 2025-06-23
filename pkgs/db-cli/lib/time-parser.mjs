const HOUR_IN_MS = 60 * 60 * 1000;
const MINUTE_IN_MS = 60 * 1000;
const RELATIVE_TIME_OFFSET = 3;

function parseRelativeTime(timeStr) {
  const parts = timeStr.slice(RELATIVE_TIME_OFFSET).match(/(\d+)\s*(hour|minute|min|h|m)/i);
  if (!parts) return null;

  const amount = parseInt(parts[1], 10);
  const unit = parts[2].toLowerCase();
  const ms = unit.startsWith('h') ? amount * HOUR_IN_MS : amount * MINUTE_IN_MS;
  return new Date(Date.now() + ms);
}

function parseTimeWithPrefix(timeStr, prefix) {
  const timeOnly = timeStr.slice(prefix.length);
  const today = new Date();
  const [hours, minutes] = timeOnly.split(':').map(Number);
  today.setHours(hours, minutes, 0, 0);
  // If the time is in the past, assume tomorrow
  if (today < new Date()) {
    today.setDate(today.getDate() + 1);
  }
  return today;
}

export function parseTime(timeStr) {
  if (!timeStr) return null;

  // Handle relative times
  if (timeStr.toLowerCase().startsWith('in ')) {
    return parseRelativeTime(timeStr);
  }

  // Handle "by HH:MM" format
  if (timeStr.toLowerCase().startsWith('by ')) {
    return parseTimeWithPrefix(timeStr, 'by ');
  }

  // Handle HH:MM format
  if (/^\d{1,2}:\d{2}$/.test(timeStr)) {
    return parseTimeWithPrefix(timeStr, '');
  }

  // Try to parse as ISO date
  const date = new Date(timeStr);
  if (!Number.isNaN(date.getTime())) {
    return date;
  }

  throw new Error(`Invalid time format: ${timeStr}`);
}

export function formatDuration(minutes) {
  const hours = Math.floor(minutes / 60);
  const mins = minutes % 60;
  return hours > 0 ? `${hours}h ${mins}min` : `${mins}min`;
}
