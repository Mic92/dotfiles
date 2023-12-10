@register_command
class FMMapCommand(GenericCommand):
    """Display a comprehensive layout of the virtual memory mapping. If a filter argument, GEF will
    filter out the mapping whose pathname do not match that filter."""

    _cmdline_ = "fmmap"
    _syntax_  = f"{_cmdline_:s} [FILTER]"
    _example_ = f"{_cmdline_:s} libc"

    @only_if_gdb_running
    def do_invoke(self, argv) -> None:
        vmmap = get_process_maps()
        if not vmmap:
            err("No address mapping information found")
            return

        if not get_gef_setting("gef.disable_color"):
            self.show_legend()

        color = get_gef_setting("theme.table_heading")

        headers = ["Start", "End", "Offset", "Perm", "Path"]
        gef_print(Color.colorify("{:<{w}s}{:<{w}s}{:<{w}s}{:<4s} {:s}".format(*headers, w=get_memory_alignment()*2+3), color))

        addr = None
        try:
            if argv:
                addr = int(argv[0], 16)
        except ValueError:
            pass

        for entry in vmmap:
            if argv:
                match = argv[0] in entry.path
                match |= entry.page_start <= addr and addr < entry.page_end
                if not match:
                    continue

            self.print_entry(entry)
        return

    def print_entry(self, entry) -> None:
        line_color = ""
        if entry.path == "[stack]":
            line_color = get_gef_setting("theme.address_stack")
        elif entry.path == "[heap]":
            line_color = get_gef_setting("theme.address_heap")
        elif entry.permission.value & Permission.READ and entry.permission.value & Permission.EXECUTE:
            line_color = get_gef_setting("theme.address_code")

        l = []
        l.append(Color.colorify(format_address(entry.page_start), line_color))
        l.append(Color.colorify(format_address(entry.page_end), line_color))
        l.append(Color.colorify(format_address(entry.offset), line_color))

        if entry.permission.value == (Permission.READ|Permission.WRITE|Permission.EXECUTE):
            l.append(Color.colorify(str(entry.permission), "underline " + line_color))
        else:
            l.append(Color.colorify(str(entry.permission), line_color))

        l.append(Color.colorify(entry.path, line_color))
        line = " ".join(l)

        gef_print(line)

    def show_legend(self) -> None:
        code_addr_color = get_gef_setting("theme.address_code")
        stack_addr_color = get_gef_setting("theme.address_stack")
        heap_addr_color = get_gef_setting("theme.address_heap")

        gef_print("[ Legend:  {} | {} | {} ]".format(Color.colorify("Code", code_addr_color),
                                                     Color.colorify("Heap", heap_addr_color),
                                                     Color.colorify("Stack", stack_addr_color)
        ))

if __name__ == "__main__":
    register_external_command( FMMapCommand() )
