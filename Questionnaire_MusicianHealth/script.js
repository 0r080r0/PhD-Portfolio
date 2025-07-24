// Database setup
let db;
let currentUser = null;
let isRegistering = false;

// Your original survey variables
let currentPage = 0;
const answers = [];
const questions = [
    {
        group: "Physical Health",
        questions: [
            "Do you experience pain or discomfort in your hands or wrists after playing?",
            "Do you experience neck or back pain after playing for extended periods?",
            "Do you feel tension or fatigue in your body after long practice sessions?",
            "Do you take regular breaks during practice sessions to stretch or rest?",
            "Do you experience any hearing issues or discomfort from loud environments?"
        ]
    },
    {
        group: "Mental Health",
        questions: [
            "Do you often feel stressed or anxious before performances?",
            "Do you struggle with self-doubt or imposter syndrome?",
            "Do you experience burnout after long rehearsals?",
            "Do you find it difficult to relax after practice?",
            "Do you have trouble sleeping due to stress or overthinking?",
            "Do you feel emotionally drained after performances?"
        ]
    },
    {
        group: "Overall Well-being",
        questions: [
            "Do you maintain a healthy diet and stay hydrated?",
            "Do you incorporate physical exercise into your routine?",
            "Do you have a support network for mental health?",
            "Do you feel balanced between music and personal time?",
            "Do you seek professional help when needed?"
        ]
    }
];

// Initialize IndexedDB
function initDB() {
    const request = indexedDB.open('MusicianHealthDB', 1);
    
    request.onerror = () => showMessage('Database error', 'error');
    
    request.onsuccess = (e) => {
        db = e.target.result;
    };
    
    request.onupgradeneeded = (e) => {
        db = e.target.result;
        
        if (!db.objectStoreNames.contains('users')) {
            db.createObjectStore('users', { keyPath: 'username' });
        }
        
        if (!db.objectStoreNames.contains('responses')) {
            const responseStore = db.createObjectStore('responses', { keyPath: 'id', autoIncrement: true });
            responseStore.createIndex('username', 'username', { unique: false });
            responseStore.createIndex('timestamp', 'timestamp', { unique: false });
        }
    };
}

// Simple hash function for passwords
async function hashPassword(password) {
    const encoder = new TextEncoder();
    const data = encoder.encode(password);
    const hash = await crypto.subtle.digest('SHA-256', data);
    return Array.from(new Uint8Array(hash)).map(b => b.toString(16).padStart(2, '0')).join('');
}

// Show message to user
function showMessage(text, type = 'success') {
    const authMessage = document.getElementById('auth-message');
    authMessage.innerHTML = `<div class="message ${type}">${text}</div>`;
    setTimeout(() => authMessage.innerHTML = '', 5000);
}

// Toggle between login and register
function toggleAuthMode() {
    isRegistering = !isRegistering;
    const submitBtn = document.getElementById('auth-submit');
    const toggleText = document.getElementById('auth-toggle-text');
    const toggleLink = document.querySelector('.auth-toggle');
    
    if (isRegistering) {
        submitBtn.textContent = 'Register';
        toggleText.textContent = 'Already have an account?';
        toggleLink.textContent = 'Login here';
    } else {
        submitBtn.textContent = 'Login';
        toggleText.textContent = "Don't have an account?";
        toggleLink.textContent = 'Register here';
    }
}

// Handle authentication
async function handleAuth() {
    const username = document.getElementById('username').value.trim();
    const password = document.getElementById('password').value;
    
    if (!username || !password) {
        showMessage('Please fill in all fields', 'error');
        return;
    }
    
    const hashedPassword = await hashPassword(password);
    
    const transaction = db.transaction(['users'], isRegistering ? 'readwrite' : 'readonly');
    const store = transaction.objectStore('users');
    
    if (isRegistering) {
        const checkRequest = store.get(username);
        checkRequest.onsuccess = () => {
            if (checkRequest.result) {
                showMessage('Username already exists', 'error');
            } else {
                const addRequest = store.add({ username, password: hashedPassword });
                addRequest.onsuccess = () => {
                    showMessage('Account created successfully!', 'success');
                    login(username);
                };
                addRequest.onerror = () => showMessage('Registration failed', 'error');
            }
        };
    } else {
        const getRequest = store.get(username);
        getRequest.onsuccess = () => {
            const user = getRequest.result;
            if (user && user.password === hashedPassword) {
                login(username);
            } else {
                showMessage('Invalid username or password', 'error');
            }
        };
    }
}

// Login user
function login(username) {
    currentUser = username;
    document.getElementById('user-name').textContent = username;
    document.getElementById('auth-page').style.display = 'none';
    document.getElementById('start-page').style.display = 'block';
}

// Logout user
function logout() {
    currentUser = null;
    document.getElementById('start-page').style.display = 'none';
    document.getElementById('dashboard-page').style.display = 'none';
    document.getElementById('tips-dashboard').style.display = 'none';
    document.getElementById('questionnaire').style.display = 'none';
    document.getElementById('thank-you-page').style.display = 'none';
    document.getElementById('auth-page').style.display = 'block';
    document.getElementById('username').value = '';
    document.getElementById('password').value = '';
}

// YOUR ORIGINAL FUNCTIONS - unchanged
function startQuiz() {
    currentPage = 0;
    answers.length = 0;
    document.getElementById('start-page').style.display = 'none';
    document.getElementById('questionnaire').style.display = 'block';
    showQuestions();
}

function showQuestions() {
    const currentGroup = questions[currentPage];
    const form = document.getElementById('health-form');
    form.innerHTML = `<h2>${currentGroup.group}</h2>`;

    currentGroup.questions.forEach((q, index) => {
        form.innerHTML += `
            <div class="question">
                <label>${q}</label><br>
                <input type="radio" name="q${index + 1}" value="yes"> Yes
                <input type="radio" name="q${index + 1}" value="no"> No
            </div>
        `;
    });

    document.getElementById('prev-button').style.display = currentPage > 0 ? 'inline-block' : 'none';
    document.getElementById('next-button').innerText = currentPage === questions.length - 1 ? 'Submit' : 'Next';
}

function changeQuestion(direction) {
    const form = document.getElementById('health-form');
    const currentGroup = questions[currentPage];

    // Collect answers for current page (modified to save data)
    const pageAnswers = [];
    currentGroup.questions.forEach((q, index) => {
        const answer = document.querySelector(`input[name="q${index + 1}"]:checked`);
        pageAnswers.push({
            question: q,
            answer: answer ? answer.value : 'no',
            group: currentGroup.group
        });
    });

    if (direction === 1) {
        answers[currentPage] = pageAnswers;
        currentPage++;
    } else {
        currentPage--;
    }

    if (currentPage < questions.length && direction === 1) {
        showQuestions();
    } else if (currentPage >= 0 && direction === -1) {
        showQuestions();
    } else if (direction === 1) {
        // Submit survey - save to database
        saveResponse();
    }
}

function showTipsPage() {
    document.getElementById('thank-you-page').style.display = 'none';
    document.getElementById('tips-dashboard').style.display = 'block';
}

function showCategoryTips(category) {
    const tipsContainer = document.getElementById('tips-container');
    tipsContainer.innerHTML = '';

    let tips;

    if (category === 'Physical Health') {
        tips = `
            <div class="fade-in">
                <h3>Physical Health Tips</h3>
                <ul>
                    <li>Take breaks every 30-45 minutes to stretch and relax your muscles.</li>
                    <li>Maintain good posture to avoid strain on your neck, back, and wrists.</li>
                    <li>Stay hydrated and eat a balanced diet to support physical health.</li>
                </ul>
            </div>
        `;
    } else if (category === 'Mental Health') {
        tips = `
            <div class="fade-in">
                <h3>Mental Health Tips</h3>
                <ul>
                    <li>Practice mindfulness or meditation to reduce stress before performances.</li>
                    <li>Set realistic goals to avoid burnout and maintain motivation.</li>
                    <li>Seek support from peers or professionals if you are feeling overwhelmed.</li>
                </ul>
            </div>
        `;
    } else {
        tips = `
            <div class="fade-in">
                <h3>Overall Well-being Tips</h3>
                <ul>
                    <li>Ensure a balance between music and personal time to avoid burnout.</li>
                    <li>Stay active and exercise regularly to support both physical and mental health.</li>
                    <li>Seek help if you experience mental or physical health challenges.</li>
                </ul>
            </div>
        `;
    }

    tipsContainer.innerHTML = tips;
}

// NEW FUNCTIONS for database functionality
function saveResponse() {
    const flatAnswers = answers.flat();
    const response = {
        username: currentUser,
        timestamp: new Date().toISOString(),
        answers: flatAnswers
    };

    const transaction = db.transaction(['responses'], 'readwrite');
    const store = transaction.objectStore('responses');
    const request = store.add(response);

    request.onsuccess = () => {
        document.getElementById('questionnaire').style.display = 'none';
        document.getElementById('thank-you-page').style.display = 'block';
    };

    request.onerror = () => {
        alert('Error saving response');
    };
}

function calculateScores(responses) {
    if (responses.length === 0) return { physical: 0, mental: 0, wellbeing: 0 };
    
    const latest = responses[0];
    const scores = { physical: 0, mental: 0, wellbeing: 0 };
    const totals = { physical: 0, mental: 0, wellbeing: 0 };
    
    latest.answers.forEach(answer => {
        const yesValue = answer.answer === 'yes' ? 1 : 0;
        if (answer.group === 'Physical Health') {
            scores.physical += yesValue;
            totals.physical++;
        } else if (answer.group === 'Mental Health') {
            scores.mental += yesValue;
            totals.mental++;
        } else if (answer.group === 'Overall Well-being') {
            scores.wellbeing += yesValue;
            totals.wellbeing++;
        }
    });
    
    return {
        physical: Math.round((scores.physical / totals.physical) * 100),
        mental: Math.round((scores.mental / totals.mental) * 100),
        wellbeing: Math.round((scores.wellbeing / totals.wellbeing) * 100)
    };
}

function getTrend(responses, category) {
    if (responses.length < 2) return 'same';
    
    const latest = calculateScoresForResponse(responses[0]);
    const previous = calculateScoresForResponse(responses[1]);
    
    const latestScore = latest[category];
    const previousScore = previous[category];
    
    if (latestScore > previousScore) return 'up';
    if (latestScore < previousScore) return 'down';
    return 'same';
}

function calculateScoresForResponse(response) {
    const scores = { physical: 0, mental: 0, wellbeing: 0 };
    const totals = { physical: 0, mental: 0, wellbeing: 0 };
    
    response.answers.forEach(answer => {
        const yesValue = answer.answer === 'yes' ? 1 : 0;
        if (answer.group === 'Physical Health') {
            scores.physical += yesValue;
            totals.physical++;
        } else if (answer.group === 'Mental Health') {
            scores.mental += yesValue;
            totals.mental++;
        } else if (answer.group === 'Overall Well-being') {
            scores.wellbeing += yesValue;
            totals.wellbeing++;
        }
    });
    
    return {
        physical: Math.round((scores.physical / totals.physical) * 100),
        mental: Math.round((scores.mental / totals.mental) * 100),
        wellbeing: Math.round((scores.wellbeing / totals.wellbeing) * 100)
    };
}

function createProgressChart(responses) {
    const ctx = document.getElementById('progressChart').getContext('2d');
    
    // Prepare data for chart
    const chartData = responses.slice().reverse().map(response => {
        const scores = calculateScoresForResponse(response);
        return {
            date: new Date(response.timestamp).toLocaleDateString(),
            physical: scores.physical,
            mental: scores.mental,
            wellbeing: scores.wellbeing
        };
    });
    
    new Chart(ctx, {
        type: 'line',
        data: {
            labels: chartData.map(d => d.date),
            datasets: [{
                label: 'Physical Health',
                data: chartData.map(d => d.physical),
                borderColor: '#6a9fb5',
                backgroundColor: 'rgba(106, 159, 181, 0.1)',
                tension: 0.4
            }, {
                label: 'Mental Health',
                data: chartData.map(d => d.mental),
                borderColor: '#4a708f',
                backgroundColor: 'rgba(74, 112, 143, 0.1)',
                tension: 0.4
            }, {
                label: 'Overall Well-being',
                data: chartData.map(d => d.wellbeing),
                borderColor: '#3e5d70',
                backgroundColor: 'rgba(62, 93, 112, 0.1)',
                tension: 0.4
            }]
        },
        options: {
            responsive: true,
            plugins: {
                legend: {
                    labels: {
                        color: '#b0c7d5'
                    }
                }
            },
            scales: {
                y: {
                    beginAtZero: true,
                    max: 100,
                    title: {
                        display: true,
                        text: 'Concern Level (%)',
                        color: '#b0c7d5'
                    },
                    ticks: {
                        color: '#b0c7d5'
                    },
                    grid: {
                        color: 'rgba(176, 199, 213, 0.1)'
                    }
                },
                x: {
                    ticks: {
                        color: '#b0c7d5'
                    },
                    grid: {
                        color: 'rgba(176, 199, 213, 0.1)'
                    }
                }
            }
        }
    });
}

function showDashboard() {
    document.getElementById('start-page').style.display = 'none';
    document.getElementById('dashboard-page').style.display = 'block';
    
    const transaction = db.transaction(['responses'], 'readonly');
    const store = transaction.objectStore('responses');
    const index = store.index('username');
    const request = index.getAll(currentUser);
    
    request.onsuccess = () => {
        const responses = request.result.sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp));
        
        if (responses.length === 0) {
            document.getElementById('physical-score').textContent = '0%';
            document.getElementById('mental-score').textContent = '0%';
            document.getElementById('wellbeing-score').textContent = '0%';
            document.getElementById('history-container').innerHTML = '<p>No survey data available yet. Take your first survey!</p>';
            return;
        }
        
        // Update score displays
        const scores = calculateScores(responses);
        document.getElementById('physical-score').textContent = scores.physical + '%';
        document.getElementById('mental-score').textContent = scores.mental + '%';
        document.getElementById('wellbeing-score').textContent = scores.wellbeing + '%';
        
        // Update trends
        const physicalTrend = getTrend(responses, 'physical');
        const mentalTrend = getTrend(responses, 'mental');
        const wellbeingTrend = getTrend(responses, 'wellbeing');
        
        updateTrendDisplay('physical-trend', physicalTrend);
        updateTrendDisplay('mental-trend', mentalTrend);
        updateTrendDisplay('wellbeing-trend', wellbeingTrend);
        
        // Create progress chart
        if (responses.length > 1) {
            createProgressChart(responses);
        }
        
        // Display history
        displayHistory(responses);
    };
}

function updateTrendDisplay(elementId, trend) {
    const element = document.getElementById(elementId);
    element.className = 'score-trend trend-' + trend;
    
    const trendText = {
        'up': '↗ Increased',
        'down': '↘ Decreased', 
        'same': '→ No change'
    };
    
    element.textContent = trendText[trend] || '→ No change';
}

function displayHistory(responses) {
    const container = document.getElementById('history-container');
    
    if (responses.length === 0) {
        container.innerHTML = '<p>No previous responses found.</p>';
        return;
    }
    
    container.innerHTML = responses.slice(0, 5).map(response => {
        const date = new Date(response.timestamp).toLocaleString();
        const yesCount = response.answers.filter(a => a.answer === 'yes').length;
        const totalQuestions = response.answers.length;
        
        // Group concerns by category and count occurrences
        const concernCounts = {};
        response.answers.filter(a => a.answer === 'yes').forEach(answer => {
            concernCounts[answer.group] = (concernCounts[answer.group] || 0) + 1;
        });
        
        const concernTags = Object.entries(concernCounts).map(([group, count]) => 
            `<span class="concern-tag">${group} (${count})</span>`
        ).join('');
        
        return `
            <div class="response-item">
                <div class="response-date">${date}</div>
                <strong>Health Concerns:</strong> ${yesCount}/${totalQuestions} questions answered "Yes"
                <br><strong>Areas of concern:</strong><br>
                ${concernTags || '<span class="concern-tag">None</span>'}
            </div>
        `;
    }).join('');
}

function showHistory() {
    showDashboard(); // Redirect to dashboard instead
}

function backToStart() {
    document.getElementById('tips-dashboard').style.display = 'none';
    document.getElementById('dashboard-page').style.display = 'none';
    document.getElementById('start-page').style.display = 'block';
}

// Initialize app
window.addEventListener('load', initDB);
