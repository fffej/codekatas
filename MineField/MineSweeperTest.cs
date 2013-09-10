using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        [Test]
        public void TestSimplestMineField()
        {
            var mineField = new MineField(".");
            Assert.That(mineField.Show(), Is.EqualTo("0"));
        }
    }

    public class MineField
    {
        public MineField(string s)
        {
            
        }

        public string Show()
        {
            return "0";
        }
    }
}
