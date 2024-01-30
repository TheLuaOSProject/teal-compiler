// Copyright (C) 2024 Amrit Bhogal
//
// This file is part of teal-compiler.
//
// teal-compiler is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// teal-compiler is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with teal-compiler.  If not, see <http://www.gnu.org/licenses/>.

#include "utilities.hpp"

#include "RawAST.hpp"

class Event {
public:
    virtual bool should_fire() = 0;
    virtual bool fire() = 0; // return true if event should be removed
    virtual ~Event() = 0;
};

class TimerEvent : public Event {
public:
    TimerEvent(std::chrono::milliseconds interval, std::function<void ()> &&callback) : _interval(interval), _callback(std::move(callback)) {}

    bool should_fire() override
    {  return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - _last_fire) >= _interval; }

    bool fire() override {
        _callback();
        _last_fire = std::chrono::high_resolution_clock::now();
        return false;
    }

private:
    std::chrono::milliseconds _interval;
    std::function<void()> _callback;
    std::chrono::high_resolution_clock::time_point _last_fire = std::chrono::high_resolution_clock::now();
};

class RunLoop {
public:
    void add_event(std::unique_ptr<Event> &&event) {
        _events.push_back(std::move(event));
    }

    void run() {
        while (true) {
            for (auto &&event : _events) {
                if (event->should_fire()) {
                    auto rm = event->fire();
                    if (rm)
                        _events.erase(std::remove_if(_events.begin(), _events.end(), [&](auto &&e) { return e.get() == event.get(); }), _events.end());
                }
            }
        }
    }

private:
    std::vector<std::unique_ptr<Event>> _events;
};

int main()
{
    RunLoop loop;
    loop.add_event(std::make_unique<TimerEvent>(std::chrono::milliseconds(1000), []() { std::println("Hello"); }));
    loop.run();

    // std::filesystem::path path = "test.tl";
    // auto contents = ({
    //     auto file = std::ifstream(path);
    //     std::string contents;
    //     file.seekg(0, std::ios::end);
    //     contents.reserve(file.tellg());
    //     file.seekg(0, std::ios::beg);
    //     contents.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    //     contents;
    // });


    // auto ast = teal::raw::Node::convert_from_lua(contents, "test.teal");
    // std::println("tk: {}, kind: {}, children.size(): {}", ast.tk.value(), magic_enum::enum_name(ast.kind.value()),  ast.children->size());
}
