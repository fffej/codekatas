using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        [Test]
        public void EmptyMineField()
        {
            var mineField = new MineField(".");
            Assert.That(mineField.Show(), Is.EqualTo("0"));
        }

        [Test]
        public void OneMine()
        {
            var mineField = new MineField("*");
            Assert.That(mineField.Show(), Is.EqualTo("*"));
        }

        [Test]
        public void OneLine()
        {
            var mineField = new MineField(".*.");
            Assert.That(mineField.Show(), Is.EqualTo("1*1"));
        }

        [Test]
        public void EmptyMineFieldWithTwoRows()
        {
            var mineField = new MineField("...\n...");
            Assert.That(mineField.Show(), Is.EqualTo("000\n000"));
        }
    }

    public class MineField
    {
        private readonly string[] m_MineField;

        public MineField(string mineField)
        {
            m_MineField = mineField.Split('\n');
        }

        public string Show()
        {
            var result = "";
            for (var line = 0; line < m_MineField.Length; ++line)
            {
                for (var i = 0; i < m_MineField[line].Length; ++i)
                {
                    var surroundingMines = 0;
                    if (i > 0 && IsMine(line, i - 1))
                    {
                        surroundingMines++;
                    }
                    if (i + 1 < m_MineField[line].Length && IsMine(line, i + 1))
                    {
                        surroundingMines++;
                    }

                    if (!IsMine(line, i))
                    {
                        result += surroundingMines;
                    }
                    else
                    {
                        result += '*';
                    }
                }
                result += "\n";
            }

            return result.TrimEnd();
        }

        private bool IsMine(int line, int index)
        {
            return m_MineField[line][index] == '*';
        }
    }
}
