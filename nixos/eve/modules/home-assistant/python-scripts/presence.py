to_state = data.get("to_state")
from_state = data.get("from_state")
entity_id = data.get("entity_id")

shannan = hass.states.get("device_tracker.beatrice")
joerg = hass.states.get("person.jorg_thalheim")
calendar = hass.states.get("calendar.joerg_shannan_jorg_thalheim")
not_together = hass.states.get("input_boolean.shannan_joerg_not_together")

shannans_home = "Shannan's Home"
joergs_home = "home"
shannans_work = "Work of Shannan"
university = "University"
gym = "Gym"


def get_message():
    logger.info(f"to_state: {to_state}")
    logger.info(f"from_state: {from_state}")
    logger.info(f"entity_id: {entity_id}")
    logger.info(f"joerg.entity_id: {joerg.entity_id}")
    logger.info(f"shannan.entity_id: {shannan.entity_id}")
    logger.info(f"home: {joergs_home}")
    logger.info(f"uni: {university}")
    logger.info(f"work: {shannans_work}")
    logger.info(f"shannans_home: {shannans_home}")
    logger.info(f"entity_id == joerg: {joerg.entity_id == entity_id}")
    logger.info(f"entity_id == shannan: {shannan.entity_id == entity_id}")

    if to_state == shannans_home:
        if entity_id == joerg.entity_id:
            return "Jörg arrived at your place"
        else:
            return "Shannan is home"
    elif to_state == joergs_home:
        if entity_id == joerg.entity_id:
            return "Jörg is home"
        else:
            return "Shannan arrived at your place"
    elif from_state == joergs_home and entity_id == joerg.entity_id:
        return "Jörg left home"
    elif from_state == shannans_home and entity_id == shannan.entity_id:
        return "Shannan left home"
    elif from_state == university and entity_id == joerg.entity_id:
        return "Jörg left Uni"
    elif from_state == shannans_work and entity_id == shannan.entity_id:
        return "Shannan left work"
    elif to_state == gym and calendar.attributes.message == "Gym":
        if entity_id == joerg.entity_id:
            return "Jörg arrived at the Gym"
        else:
            return "Shannan arrived at the Gym"
    return None


def main():
    message = get_message()
    logger.info(f"message {message}")
    if message is None:
        return
    if not_together == "off":
        logger.info("skip notification, shannan and joerg are together")
        return
    if entity_id == joerg.entity_id:
        notify_service = "mobile_app_beatrice"
    else:
        notify_service = "pushover"

    logger.info(f"notify_service: {notify_service}")
    hass.services.call("notify", notify_service, {"message": message}, blocking=False)

    if joerg.state == shannan.state:
        logger.info(f"shannan and joerg are together now")
        hass.services.call(
            "input_boolean",
            "turn_off",
            {"entity_id": "shannan_joerg_not_together"},
            blocking=False,
        )


main()
