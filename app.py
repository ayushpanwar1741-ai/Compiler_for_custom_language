from flask import Flask, render_template, request, jsonify
import subprocess
import tempfile
import os

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/rules')
def rules():
    return render_template('rulesection.html')

@app.route('/compile', methods=['POST'])
def compile_code():
    data = request.get_json()
    code = data.get('code')
    action = data.get('action')
    if not code or not action:
        return jsonify({'output': 'Missing code or action.'}), 400

    filename = None
    try:
        with tempfile.NamedTemporaryFile(delete=False, suffix='.txt', mode='w') as f:
            f.write(code)
            filename = f.name
        result = subprocess.run(['./compiler', filename], capture_output=True, text=True, timeout=10)
        output = result.stdout
    except Exception as e:
        output = f"Error running compiler: {e}"
    finally:
        if filename and os.path.exists(filename):
            os.unlink(filename)

    section_titles = {
        "tokenize": "Lexical Analysis (Tokenization)",
        "parse": "Syntax Analysis (Parsing)",
        "semantic": "Semantic Analysis",
        "intermediate": "Intermediate Code Generation",
        "assembly": "Assembly Code Generation",
    }
    section_title = section_titles.get(action)
    if action == 'optimize':
        try:
            with tempfile.NamedTemporaryFile(delete=False, suffix='.txt', mode='w') as f:
                f.write(code)
                filename = f.name
            result = subprocess.run(['./compiler', filename, '--optimize'], capture_output=True, text=True, timeout=10)
            output = result.stdout
            # Parse optimized code and techniques
            opt_code = ''
            techniques = ''
            if '=== Optimized Code ===' in output and '=== Optimization Techniques Used ===' in output:
                parts = output.split('=== Optimized Code ===')
                if len(parts) > 1:
                    code_and_tech = parts[1].split('=== Optimization Techniques Used ===')
                    if len(code_and_tech) > 1:
                        opt_code = code_and_tech[0].strip()
                        techniques = code_and_tech[1].strip()
            return jsonify({'output': 'Optimization complete!', 'optimized_code': opt_code, 'technique': techniques})
        except Exception as e:
            return jsonify({'output': f'Error running optimizer: {e}', 'optimized_code': '', 'technique': ''})
        finally:
            if filename and os.path.exists(filename):
                os.unlink(filename)
    elif section_title:
        output = extract_section(output, section_title)
    else:
        output = "Invalid action requested."

    return jsonify({'output': output})

def extract_section(text, section_title):
    lines = text.splitlines()
    start = None
    for i, line in enumerate(lines):
        if section_title in line:
            start = i
            break
    if start is None:
        return f"Section '{section_title}' not found."
    end = len(lines)
    for j in range(start + 1, len(lines)):
        if "===" in lines[j] and j != start:
            end = j
            break
    return "\n".join(lines[start:end])

if __name__ == '__main__':
    app.run(debug=True)
