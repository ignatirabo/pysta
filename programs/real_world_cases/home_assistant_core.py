CHECK_EMPTY = ["Cannot be empty", lambda value: value]

def gather_info(arguments) -> Info:
    """Gather info."""
    if arguments.integration:
        info = {"domain": arguments.integration}
    elif arguments.develop:
        print("Running in developer mode. Automatically filling in info.")
        print()
        info = {"domain": "develop"}
    else:
        info = _gather_info(
            {
                "domain": {
                    "prompt": "What is the domain?",
                    "validators": [
                        CHECK_EMPTY,
                        [
                            "Domains cannot contain spaces or special characters.",
                            lambda value: value == slugify(value),
                        ],
                    ],
                }
            }
        )

    info["is_new"] = True
    # info["is_new"] = not (COMPONENT_DIR / info["domain"] / "manifest.json").exists()

    if not info["is_new"]:
        return _load_existing_integration(info["domain"])

    if arguments.develop:
        info.update(
            {
                "name": "Develop Hub",
                "codeowner": "@developer",
                "requirement": "aiodevelop==1.2.3",
                "oauth2": True,
                "iot_class": "local_polling",
            }
        )
    else:
        info.update(gather_new_integration(arguments.template == "integration"))

    return Info(**info)

YES_NO = {
    "validators": [["Type either 'yes' or 'no'", lambda value: value in ("yes", "no")]],
    "converter": lambda value: value == "yes",
}


def gather_new_integration(determine_auth: bool) -> Info:
    """Gather info about new integration from user."""
    fields = {
        "name": {
            "prompt": "What is the name of your integration?",
            "validators": [CHECK_EMPTY],
        },
        "codeowner": {
            "prompt": "What is your GitHub handle?",
            "validators": [
                CHECK_EMPTY,
                [
                    'GitHub handles need to start with an "@"',
                    lambda value: value.startswith("@"),
                ],
            ],
        },
        "requirement": {
            "prompt": "What PyPI package and version do you depend on? Leave blank for none.",
            "validators": [
                [
                    "Versions should be pinned using '=='.",
                    lambda value: not value or "==" in value,
                ]
            ],
        },
        "iot_class": {
            "prompt": (
                f"""How will your integration gather data?

Valid values are {', '.join(SUPPORTED_IOT_CLASSES)}

More info @ https://developers.home-assistant.io/docs/creating_integration_manifest#iot-class
"""
            ),
            "validators": [
                [
                    f"You need to pick one of {', '.join(SUPPORTED_IOT_CLASSES)}",
                    lambda value: value in SUPPORTED_IOT_CLASSES,
                ]
            ],
        },
    }

    if determine_auth:
        fields.update(
            {
                "authentication": {
                    "prompt": "Does Home Assistant need the user to authenticate to control the device/service? (yes/no)",
                    "default": "yes",
                    # **YES_NO,
                },
                "discoverable": {
                    "prompt": "Is the device/service discoverable on the local network? (yes/no)",
                    "default": "no",
                    # **YES_NO,
                },
                "helper": {
                    "prompt": "Is this a helper integration? (yes/no)",
                    "default": "no",
                    # **YES_NO,
                },
                "oauth2": {
                    "prompt": "Can the user authenticate the device using OAuth2? (yes/no)",
                    "default": "no",
                    # **YES_NO,
                },
            }
        )

    return _gather_info(fields)

def main():
    """Scaffold an integration."""
    # path = Path("requirements_all.txt")
    # if not path.is_file():
    #     print("Run from project root")
    #     return 1

    # In Pysta, this function should create a new symbol
    args = get_arguments()

    info = gather_info.gather_info(args)
    print()

    # If we are calling scaffold on a non-existing integration,
    # We're going to first make it. If we're making an integration,
    # we will also make a config flow to go with it.

    if info.is_new:
        generate.generate("integration", info)

        # If it's a new integration and it's not a config flow,
        # create a config flow too.
        if not args.template.startswith("config_flow"):
            if info.helper:
                template = "config_flow_helper"
            elif info.oauth2:
                template = "config_flow_oauth2"
            elif info.authentication or not info.discoverable:
                template = "config_flow"
            else:
                template = "config_flow_discovery"

            generate.generate(template, info)

    # If we wanted a new integration, we've already done our work.
    if args.template != "integration":
        generate.generate(args.template, info)

    pipe_null = {}
    # pipe_null = {} if args.develop else {"stdout": subprocess.DEVNULL}

    print("Running hassfest to pick up new information.")
    subprocess.run(["python", "-m", "script.hassfest"], **pipe_null, check=True)
    print()

    print("Running gen_requirements_all to pick up new information.")
    subprocess.run(
        ["python", "-m", "script.gen_requirements_all"], **pipe_null, check=True
    )
    print()

    print("Running script/translations_develop to pick up new translation strings.")
    subprocess.run(
        [
            "python",
            "-m",
            "script.translations",
            "develop",
            "--integration",
            info.domain,
        ],
        **pipe_null,
        check=True,
    )
    print()

    if args.develop:
        print("Running tests")
        print(f"$ python3 -b -m pytest -vvv tests/components/{info.domain}")
        subprocess.run(
            [
                "python3",
                "-b",
                "-m",
                "pytest",
                "-vvv",
                f"tests/components/{info.domain}",
            ],
            check=True,
        )
        print()

    docs.print_relevant_docs(args.template, info)

    return 0
